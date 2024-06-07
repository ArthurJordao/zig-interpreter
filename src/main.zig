const std = @import("std");
const mecha = @import("mecha");

const ValType = enum {
    num,
    expr,
    symbol,
};

const Error = enum {
    divisionByZero,
    arityMismatch,
    invalidOperator,
    invalidOperand,
};

const Val = union(ValType) {
    num: i32,
    expr: Expr,
    symbol: []u8,
};

const Expr = []Val;

const EvaluationType = enum {
    num,
    runtimeError,
};

const EvaluationValue = union(EvaluationType) {
    num: i32,
    runtimeError: Error,
};

const value = mecha.combine(.{
    mecha.oneOf(.{
        mecha.int(i32, .{
            .parse_sign = false,
            .base = 10,
        }).map(numToValue),
        symbol,
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

const allowedSymbolSpecialCharacters = mecha.combine(.{
    mecha.oneOf(.{
        mecha.ascii.char('+'),
        mecha.ascii.char('-'),
        mecha.ascii.char('/'),
        mecha.ascii.char('*'),
    }),
});

const uppercaseCharacter = mecha.ascii.range('A', 'Z');
const lowercaseCharacter = mecha.ascii.range('a', 'z');
const numberCharacter = mecha.ascii.range('0', '9');

const symbol = mecha.combine(.{
    mecha.oneOf(.{
        uppercaseCharacter,
        lowercaseCharacter,
        allowedSymbolSpecialCharacters,
    }),
    mecha.oneOf(.{
        uppercaseCharacter,
        lowercaseCharacter,
        allowedSymbolSpecialCharacters,
        numberCharacter,
    }).many(.{ .collect = true }),
    ws,
}).convert(parseSymbol);

fn parseSymbol(allocator: std.mem.Allocator, parsedValue: std.meta.Tuple(&.{ u8, []u8 })) !Val {
    const s = try allocator.alloc(u8, parsedValue[1].len + 1);
    s[0] = parsedValue[0];
    for (1.., parsedValue[1]) |i, c| {
        s[i] = c;
    }
    allocator.free(parsedValue[1]);
    return Val{ .symbol = s };
}

const lparens = mecha.combine(.{ mecha.ascii.char('(').discard(), ws });

const rparens = mecha.combine(.{ mecha.ascii.char(')').discard(), ws });

const lisp = mecha.combine(.{
    ws,
    lparens,
    value.many(.{ .collect = true }),
    rparens,
});

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
        defer freeExpr(&allocator, ast);
        try writer.print("{any}\n", .{eval(ast)});
    }
}

fn freeExpr(allocator: *const std.mem.Allocator, expr: Expr) void {
    for (expr) |arg| {
        switch (arg) {
            .num => {},
            .expr => |e| {
                freeExpr(allocator, e);
            },
            .symbol => |sym| {
                allocator.free(sym);
            },
        }
    }
    allocator.free(expr);
}

fn eval(expr: Expr) EvaluationValue {
    switch (expr[0]) {
        .symbol => |operator| {
            if (std.mem.eql(u8, operator, "+")) {
                return evalAdd(expr[1..]);
            }
        },
        else => {
            return EvaluationValue{ .runtimeError = Error.invalidOperator };
        },
    }
    return EvaluationValue{ .runtimeError = Error.invalidOperator };
}

fn evalAdd(expr: Expr) EvaluationValue {
    var sum: i32 = 0;
    for (expr) |arg| {
        switch (arg) {
            .num => |num| {
                sum += num;
            },
            .expr => |e| {
                switch (eval(e)) {
                    .num => |num| {
                        sum += num;
                    },
                    .runtimeError => |rError| {
                        return EvaluationValue{ .runtimeError = rError };
                    },
                }
            },
            .symbol => |_| {
                return EvaluationValue{
                    .runtimeError = Error.invalidOperand,
                };
            },
        }
    }
    return EvaluationValue{ .num = sum };
}

//
// test "lisp with simple expr" {
//     const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2)")).value;
//     const evaluated = try eval(&ast, &std.testing.allocator);
//     defer freeExpr(&std.testing.allocator, &ast);
//
//     try std.testing.expectEqualDeep(
//         EvaluationValue{ .num = 3 },
//         evaluated,
//     );
// }
//
// test "lisp with recursive expr" {
//     const ast = (try lisp.parse(std.testing.allocator, "(+ 1 2 (+ 1 2))")).value;
//     const evaluated = try eval(&ast, &std.testing.allocator);
//     defer freeExpr(&std.testing.allocator, &ast);
//
//     try std.testing.expectEqualDeep(
//         EvaluationValue{ .num = 6 },
//         evaluated,
//     );
// }
