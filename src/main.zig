const std = @import("std");
const mecha = @import("mecha");

const ValType = enum {
    num,
    expr,
};

const Val = union(ValType) {
    num: *i32,
    expr: *Expr,
};

const Expr = struct {
    func: u8,
    args: []*Val,
};

const value = mecha.combine(.{
    mecha.oneOf(.{
        mecha.int(i32, .{
            .parse_sign = false,
            .base = 10,
        }).convert(numToValue),
        mecha.ref(lispRef).convert(exprToValue),
    }),
    ws,
});

fn numToValue(allocator: std.mem.Allocator, num: i32) !*Val {
    const allocatedNum = try allocator.create(i32);
    allocatedNum.* = num;
    const val = try allocator.create(Val);
    val.* = .{ .num = allocatedNum };
    return val;
}

fn exprToValue(allocator: std.mem.Allocator, expr: Expr) !*Val {
    const allocatedExpr = try allocator.create(Expr);
    allocatedExpr.* = .{
        .func = expr.func,
        .args = expr.args,
    };
    const val = try allocator.create(Val);
    val.* = .{ .expr = allocatedExpr };
    return val;
}

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

fn lispRef() mecha.Parser(Expr) {
    return lisp;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const reader = std.io.getStdIn().reader();
    const writer = std.io.getStdOut().writer();
    const input = try allocator.alloc(u8, 1024);
    defer allocator.free(input);

    while (try reader.readUntilDelimiterOrEof(input, '\n')) |line| {
        const ast = (try lisp.parse(allocator, line)).value;
        defer freeExpr(&allocator, &ast);
        try writer.print("{any}\n", .{eval(&ast, &allocator)});
    }
}

fn freeExpr(allocator: *const std.mem.Allocator, expr: *const Expr) void {
    for (expr.args) |arg| {
        switch (arg.*) {
            .num => allocator.destroy(arg.num),
            .expr => {
                freeExpr(allocator, arg.expr);
                allocator.destroy(arg.expr);
            },
        }
        allocator.destroy(arg);
    }
    allocator.free(expr.args);
}

fn eval(expr: *const Expr, allocator: *const std.mem.Allocator) !i32 {
    const evaluatedArgs = try allocator.alloc(i32, expr.args.len);
    defer allocator.free(evaluatedArgs);
    for (0.., expr.args) |i, arg| {
        switch (arg.*) {
            .num => |v| {
                evaluatedArgs[i] = v.*;
            },
            .expr => |v| {
                evaluatedArgs[i] = try eval(v, allocator);
            },
        }
    }
    switch (expr.func) {
        '+' => {
            var result: i32 = 0;
            for (evaluatedArgs) |arg| {
                result += arg;
            }
            return result;
        },
        '-' => {
            var result: i32 = evaluatedArgs[0];
            for (evaluatedArgs[1..]) |arg| {
                result -= arg;
            }
            return result;
        },
        '*' => {
            var result: i32 = 1;
            for (evaluatedArgs) |arg| {
                result *= arg;
            }
            return result;
        },
        '/' => {
            var result: i32 = evaluatedArgs[0];
            for (evaluatedArgs[1..]) |arg| {
                result = @divTrunc(result, arg);
            }
            return result;
        },
        else => return 0,
    }
}

test "lisp with simple expr" {
    const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2)")).value;
    const evaluated = try eval(&ast, &std.testing.allocator);
    defer freeExpr(&std.testing.allocator, &ast);

    try std.testing.expectEqualDeep(
        3,
        evaluated,
    );
}

test "lisp with recursive expr" {
    const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2 (+ 1 2))")).value;
    const evaluated = try eval(&ast, &std.testing.allocator);
    defer freeExpr(&std.testing.allocator, &ast);

    try std.testing.expectEqualDeep(
        6,
        evaluated,
    );
}
