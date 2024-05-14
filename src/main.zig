const std = @import("std");
const mecha = @import("mecha");

const ValType = enum {
    num,
    expr,
    runtimeError,
};

const Error = enum {
    divisionByZero,
    arityMismatch,
    invalidOperator,
};

const Val = union(ValType) {
    num: i32,
    expr: Expr,
    runtimeError: Error,
};

const Expr = struct {
    func: u8,
    args: []Val,
};

const value = mecha.combine(.{
    mecha.oneOf(.{
        mecha.int(i32, .{
            .parse_sign = false,
            .base = 10,
        }).map(numToValue),
        mecha.ref(lispRef).map(exprToValue),
    }),
    ws,
});

fn numToValue(num: i32) Val {
    return Val{ .num = num };
}

fn exprToValue(expr: Expr) Val {
    return Val{ .expr = expr };
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
        switch (arg) {
            .num => {},
            .expr => {
                freeExpr(allocator, &arg.expr);
            },
            .runtimeError => unreachable,
        }
    }
    allocator.free(expr.args);
}

fn eval(expr: *const Expr, allocator: *const std.mem.Allocator) !Val {
    const evaluatedArgs = try allocator.alloc(i32, expr.args.len);
    defer allocator.free(evaluatedArgs);
    for (0.., expr.args) |i, arg| {
        switch (arg) {
            .num => |v| {
                evaluatedArgs[i] = v;
            },
            .expr => |v| {
                const evaluated = try eval(&v, allocator);
                switch (evaluated) {
                    .num => |n| {
                        evaluatedArgs[i] = n;
                    },
                    // NOTE: This is unreachable because we are evaluating the inner expression first.
                    .expr => unreachable,
                    .runtimeError => {
                        return evaluated;
                    },
                }
            },
            .runtimeError => {
                return arg;
            },
        }
    }
    switch (expr.func) {
        '+' => {
            var result: i32 = 0;
            for (evaluatedArgs) |arg| {
                result += arg;
            }
            return Val{ .num = result };
        },
        '-' => {
            var result: i32 = evaluatedArgs[0];
            for (evaluatedArgs[1..]) |arg| {
                result -= arg;
            }
            return Val{ .num = result };
        },
        '*' => {
            var result: i32 = 1;
            for (evaluatedArgs) |arg| {
                result *= arg;
            }
            return Val{ .num = result };
        },
        '/' => {
            if (evaluatedArgs.len < 2) {
                return Val{ .runtimeError = .arityMismatch };
            }
            if (evaluatedArgs[1] == 0) {
                return Val{ .runtimeError = .divisionByZero };
            }
            var result: i32 = evaluatedArgs[0];
            for (evaluatedArgs[1..]) |arg| {
                result = @divTrunc(result, arg);
            }
            return Val{ .num = result };
        },
        else => return Val{ .runtimeError = .invalidOperator },
    }
}

test "lisp with simple expr" {
    const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2)")).value;
    const evaluated = try eval(&ast, &std.testing.allocator);
    defer freeExpr(&std.testing.allocator, &ast);

    try std.testing.expectEqualDeep(
        Val{ .num = 3 },
        evaluated,
    );
}

test "lisp with recursive expr" {
    const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2 (+ 1 2))")).value;
    const evaluated = try eval(&ast, &std.testing.allocator);
    defer freeExpr(&std.testing.allocator, &ast);

    try std.testing.expectEqualDeep(
        Val{ .num = 6 },
        evaluated,
    );
}
