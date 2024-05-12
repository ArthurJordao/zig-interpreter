const std = @import("std");
const mecha = @import("mecha");

const Expr = struct {
    operand: u8,
    left: u32,
    right: u32,
};

const value = mecha.int(u32, .{
    .parse_sign = false,
    .base = 10,
});

const ws = mecha.oneOf(.{
    mecha.utf8.char(0x0020),
    mecha.utf8.char(0x000A),
    mecha.utf8.char(0x000D),
    mecha.utf8.char(0x0009),
    mecha.utf8.char(','),
}).many(.{ .collect = false }).discard();

const operator = mecha.oneOf(.{
    mecha.ascii.char('+'),
    mecha.ascii.char('-'),
    mecha.ascii.char('/'),
    mecha.ascii.char('*'),
});

const lparens = mecha.combine(.{ mecha.ascii.char('(').discard(), ws });

const rparens = mecha.combine(.{ mecha.ascii.char(')').discard(), ws });

const lisp = mecha.combine(.{
    ws,
    lparens,
    ws,
    operator,
    ws,
    value,
    ws,
    value,
    ws,
    rparens,
    ws,
});

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const reader = std.io.getStdIn().reader();
    const writer = std.io.getStdOut().writer();
    const input = try allocator.alloc(u8, 1024);
    defer allocator.free(input);

    while (try reader.readUntilDelimiterOrEof(input, '\n')) |line| {
        const ast = (try lisp.parse(allocator, line)).value;
        try writer.print("{any}\n", .{ast});
    }
}

test "lisp" {}
