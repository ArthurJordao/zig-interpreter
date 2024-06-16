const std = @import("std");
const parser = @import("parser.zig");

const Error = enum {
    divisionByZero,
    arityMismatch,
    invalidOperator,
    invalidOperand,
    undefinedVariable,
};

const EvaluationType = enum {
    num,
    runtimeError,
    nil,
    booleanVal,
};

const EvaluationValue = union(EvaluationType) {
    num: i32,
    runtimeError: Error,
    nil: void,
    booleanVal: bool,
};

const Scope = std.StringHashMap(EvaluationValue);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var reader = std.io.getStdIn().reader();
    var writer = std.io.getStdOut().writer();
    try app(allocator, &reader, &writer);
}

fn app(allocator: std.mem.Allocator, reader: *std.fs.File.Reader, writer: *std.fs.File.Writer) !void {
    const input = try allocator.alloc(u8, 1024);
    defer allocator.free(input);

    var globalScope = Scope.init(allocator);
    defer globalScope.deinit();
    defer freeScope(&globalScope, allocator);
    while (try reader.readUntilDelimiterOrEof(input, '\n')) |line| {
        const ast = (try parser.value.parse(allocator, line)).value;
        defer freeValue(allocator, ast);
        try writer.print("{any}\n", .{eval(ast, &globalScope, allocator)});
    }
}

fn freeScope(scope: *Scope, allocator: std.mem.Allocator) void {
    var iterator = Scope.iterator(scope);
    while (iterator.next()) |entry| {
        allocator.free(entry.key_ptr.*);
    }
}

fn freeValue(allocator: std.mem.Allocator, value: parser.Val) void {
    switch (value) {
        .expr => |expr| {
            freeExpr(allocator, expr);
        },
        .symbol => |sym| {
            allocator.free(sym);
        },
        .num => {},
        .booleanVal => {},
    }
}

fn freeExpr(allocator: std.mem.Allocator, expr: parser.Expr) void {
    for (expr) |value| {
        freeValue(allocator, value);
    }
    allocator.free(expr);
}

fn newChildScope(scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!Scope {
    var newScope = Scope.init(allocator);
    var iterator = Scope.iterator(scope);
    while (iterator.next()) |entry| {
        const key = try allocator.alloc(u8, entry.key_ptr.len);
        std.mem.copyForwards(u8, key, entry.key_ptr.*);
        try newScope.put(key, entry.value_ptr.*);
    }
    return newScope;
}

fn eval(value: parser.Val, scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationValue {
    switch (value) {
        .expr => |expr| {
            return evalExpr(expr, scope, allocator);
        },
        .num => |num| {
            return EvaluationValue{ .num = num };
        },
        .symbol => |sym| {
            return (scope.get(sym)) orelse EvaluationValue{ .runtimeError = Error.undefinedVariable };
        },
        .booleanVal => |b| {
            return EvaluationValue{ .booleanVal = b };
        },
    }
}

fn evalExpr(expr: parser.Expr, scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationValue {
    if (expr.len == 0) {
        return EvaluationValue{ .nil = {} };
    }
    switch (expr[0]) {
        .symbol => |operator| {
            if (std.mem.eql(u8, operator, "+")) {
                return (try evalAdd(expr[1..], scope, allocator));
            }
            if (std.mem.eql(u8, operator, "let")) {
                var letScope = try newChildScope(scope, allocator);
                defer letScope.deinit();
                defer freeScope(&letScope, allocator);
                return (try evalLet(expr[1..], &letScope, allocator));
            }
            if (std.mem.eql(u8, operator, "def")) {
                return try evalDef(expr[1..], scope, allocator);
            }
            return EvaluationValue{ .runtimeError = Error.invalidOperator };
        },
        else => {
            return EvaluationValue{ .runtimeError = Error.invalidOperator };
        },
    }
    return EvaluationValue{ .runtimeError = Error.invalidOperator };
}

fn addToScope(scope: *Scope, key: []u8, evaluation: EvaluationValue, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    const k = try allocator.alloc(u8, key.len);
    std.mem.copyForwards(u8, k, key);
    try scope.put(k, evaluation);
}

fn evalAdd(expr: parser.Expr, scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationValue {
    var sum: i32 = 0;
    for (expr) |arg| {
        switch (arg) {
            .num => |num| {
                sum += num;
            },
            .expr => |e| {
                switch (try evalExpr(e, scope, allocator)) {
                    .num => |num| {
                        sum += num;
                    },
                    .runtimeError => |rError| {
                        return EvaluationValue{ .runtimeError = rError };
                    },
                    .nil => {},
                    .booleanVal => {
                        return EvaluationValue{ .runtimeError = Error.invalidOperand };
                    },
                }
            },
            .symbol => |sym| {
                switch (((scope.get(sym)) orelse EvaluationValue{ .runtimeError = Error.undefinedVariable })) {
                    .num => |num| {
                        sum += num;
                    },
                    .nil => {},
                    .runtimeError => |rError| {
                        return EvaluationValue{ .runtimeError = rError };
                    },
                    .booleanVal => {
                        return EvaluationValue{ .runtimeError = Error.invalidOperand };
                    },
                }
            },
            .booleanVal => {
                return EvaluationValue{ .runtimeError = Error.invalidOperand };
            },
        }
    }
    return EvaluationValue{ .num = sum };
}

fn evalDef(expr: parser.Expr, scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationValue {
    if (expr.len != 2) {
        return EvaluationValue{ .runtimeError = Error.arityMismatch };
    }
    switch (expr[0]) {
        .symbol => |sym| {
            const evaluation = try eval(expr[1], scope, allocator);
            switch (evaluation) {
                .num => {
                    try addToScope(scope, sym, evaluation, allocator);
                },
                .nil => {
                    try addToScope(scope, sym, evaluation, allocator);
                },
                .runtimeError => {
                    return EvaluationValue{ .runtimeError = evaluation.runtimeError };
                },
                .booleanVal => {
                    try addToScope(scope, sym, evaluation, allocator);
                },
            }
        },
        else => {
            return EvaluationValue{ .runtimeError = Error.invalidOperand };
        },
    }
    return EvaluationValue{ .nil = {} };
}

fn evalLet(expr: parser.Expr, scope: *Scope, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationValue {
    if (expr.len < 2) {
        return EvaluationValue{ .nil = {} };
    }
    switch (expr[0]) {
        .expr => |e| {
            if (e.len % 2 != 0) {
                return EvaluationValue{ .runtimeError = Error.arityMismatch };
            }
            for (0..e.len / 2) |i| {
                const key = e[i * 2];
                const val = e[i * 2 + 1];
                const variableName = switch (key) {
                    .symbol => |s| s,
                    .num => {
                        return EvaluationValue{ .runtimeError = Error.invalidOperand };
                    },
                    .expr => {
                        return EvaluationValue{ .runtimeError = Error.invalidOperand };
                    },
                    .booleanVal => {
                        return EvaluationValue{ .runtimeError = Error.invalidOperand };
                    },
                };
                const evaluation = try eval(val, scope, allocator);
                switch (evaluation) {
                    .num => {
                        try addToScope(scope, variableName, evaluation, allocator);
                    },
                    .runtimeError => {
                        return EvaluationValue{ .runtimeError = evaluation.runtimeError };
                    },
                    .nil => {
                        try addToScope(scope, variableName, evaluation, allocator);
                    },
                    .booleanVal => {
                        try addToScope(scope, variableName, evaluation, allocator);
                    },
                }
            }
            return try eval(expr[1], scope, allocator);
        },
        else => {
            return EvaluationValue{ .runtimeError = Error.invalidOperand };
        },
    }
}

test "lisp with simple expr" {
    const allocator = std.testing.allocator;
    const ast = (try parser.value.parse(allocator, "(+ 1 2)")).value;
    var scope = Scope.init(allocator);
    defer scope.deinit();
    const evaluated = try eval(ast, &scope, allocator);
    defer freeValue(std.testing.allocator, ast);

    try std.testing.expectEqualDeep(
        EvaluationValue{ .num = 3 },
        evaluated,
    );
}

test "lisp with recursive expr" {
    const allocator = std.testing.allocator;
    const ast = (try parser.value.parse(allocator, "(+ 1 2 (+ 1 2))")).value;
    var scope = Scope.init(allocator);
    defer scope.deinit();
    const evaluated = try eval(ast, &scope, allocator);
    defer freeValue(std.testing.allocator, ast);

    try std.testing.expectEqualDeep(
        EvaluationValue{ .num = 6 },
        evaluated,
    );
}

test "lisp with some lets" {
    const allocator = std.testing.allocator;
    const ast = (try parser.value.parse(allocator, "(let (x 1 y (let (z 3) (+ z 4))) (+ x y))")).value;
    var scope = Scope.init(allocator);
    defer scope.deinit();
    const evaluated = try eval(ast, &scope, allocator);
    defer freeValue(std.testing.allocator, ast);

    try std.testing.expectEqualDeep(
        EvaluationValue{ .num = 8 },
        evaluated,
    );
}

test "lisp with some lets and undefined variable" {
    const allocator = std.testing.allocator;
    const ast = (try parser.value.parse(allocator, "(let (x 1 y (let (z 3) (+ z 4))) (+ x y z))")).value;
    var scope = Scope.init(allocator);
    defer scope.deinit();
    const evaluated = try eval(ast, &scope, allocator);
    defer freeValue(std.testing.allocator, ast);
    try std.testing.expectEqualDeep(
        EvaluationValue{ .runtimeError = Error.undefinedVariable },
        evaluated,
    );
}
