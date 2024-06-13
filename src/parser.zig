const std = @import("std");
const mecha = @import("mecha");

pub const Expr = []Val;

pub const Val = union(ValType) {
    num: i32,
    expr: Expr,
    symbol: []u8,
};

const ValType = enum {
    num,
    expr,
    symbol,
};

pub const value = mecha.combine(.{
    mecha.oneOf(.{
        mecha.int(i32, .{
            .parse_sign = false,
            .base = 10,
        }).map(numToValue),
        symbol,
        mecha.ref(exprParserRef).map(exprToValue),
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

const exprParser = mecha.combine(.{
    ws,
    lparens,
    value.many(.{ .collect = true }),
    rparens,
});

fn exprParserRef() mecha.Parser(Expr) {
    return exprParser;
}
