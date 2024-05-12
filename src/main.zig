const std = @import("std");
const mecha = @import("mecha");

const Expr = struct {
    func: u8,
    args: []u32,
};

const value = mecha.combine(.{
    mecha.int(u32, .{
        .parse_sign = false,
        .base = 10,
    }),
    ws,
});

const ws = mecha.oneOf(.{
    mecha.utf8.char(0x0020),
    mecha.utf8.char(0x000A),
    mecha.utf8.char(0x000D),
    mecha.utf8.char(0x0009),
    mecha.utf8.char(','),
}).many(.{ .collect = false }).discard();

const operator = mecha.combine(.{
    mecha.oneOf(.{
        mecha.ascii.char('+'),
        mecha.ascii.char('-'),
        mecha.ascii.char('/'),
        mecha.ascii.char('*'),
    }),
    ws,
});

const lparens = mecha.combine(.{ mecha.ascii.char('(').discard(), ws });

const rparens = mecha.combine(.{ mecha.ascii.char(')').discard(), ws });

const lisp = mecha.combine(.{
    ws,
    lparens,
    operator,
    value.many(.{ .collect = true }),
    rparens,
}).map(mecha.toStruct(Expr));

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const reader = std.io.getStdIn().reader();
    const writer = std.io.getStdOut().writer();
    const input = try allocator.alloc(u8, 1024);
    defer allocator.free(input);

    while (try reader.readUntilDelimiterOrEof(input, '\n')) |line| {
        const ast = (try lisp.parse(allocator, line)).value;
        try writer.print("{any}\n", .{eval(&ast)});
    }
}

pub fn eval(expr: *const Expr) u32 {
    switch (expr.func) {
        '+' => {
            var result: u32 = 0;
            for (expr.args) |arg| {
                result += arg;
            }
            return result;
        },
        '-' => {
            var result: u32 = expr.args[0];
            for (expr.args) |arg| {
                result -= arg;
            }
            return result;
        },
        '*' => {
            var result: u32 = 1;
            for (expr.args) |arg| {
                result *= arg;
            }
            return result;
        },
        '/' => {
            var result: u32 = expr.args[0];
            for (expr.args) |arg| {
                result /= arg;
            }
            return result;
        },
        else => return 0,
    }
}

test "lisp" {
    const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2)")).value;
    try std.testing.expectEqual(
        '+',
        ast.func,
    );

    const expectedArgs = [_]u32{ 1, 2 };

    try std.testing.expectEqualDeep(
        &expectedArgs,
        ast.args,
    );
    std.testing.allocator.free(ast.args);
}
