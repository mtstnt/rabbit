const std = @import("std");

const Self = @This();

pub const TokenType = union(enum) {
    IDENT: std.ArrayList(u8),

    LIT_NUMBER: f64,
    LIT_STRING: std.ArrayList(u8),
    OP_PLUS: u8,
    OP_DASH: u8,
    OP_ASTERISK: u8,
    OP_SLASH: u8,
    OP_PCNTG: u8,
    OP_ASSIGN: u8,

    OP_PLUS_ASSGN: [2]u8,
    OP_DASH_ASSGN: [2]u8,
    OP_ASTERISK_ASSGN: [2]u8,
    OP_SLASH_ASSGN: [2]u8,
    OP_PCNTG_ASSGN: [2]u8,

    OP_LT: u8,
    OP_LTE: [2]u8,
    OP_GT: u8,
    OP_GTE: [2]u8,
    OP_EXCLMTN: u8,
    OP_EQUALS: [2]u8,

    SYM_LBRACE: u8,
    SYM_RBRACE: u8,
    SYM_LPAREN: u8,
    SYM_RPAREN: u8,
    SYM_LBRACKET: u8,
    SYM_RBRACKET: u8,

    SYM_COLON: u8,

    // Keywords
};

pub fn destroyTokens(tokens: *std.ArrayList(TokenType)) void {
    for (tokens.items) |token| {
        switch (token) {
            .LIT_STRING => token.LIT_STRING.deinit(),
            .IDENT => token.IDENT.deinit(),
            else => {},
        }
    }
    tokens.deinit();
}

// Lexer:
// (string/slice of string) -> (Token[])
pub fn lex(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(TokenType) {
    var pos: usize = 0;
    var tokens = std.ArrayList(TokenType).init(allocator);
    errdefer destroyTokens(&tokens);

    while (true) {
        if (pos >= source.len) break;
        const current = source[pos];
        switch (current) {
            // Operators
            '+' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_PLUS_ASSGN = [2]u8{ '+', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_PLUS = '+' });
                    pos += 1;
                }
            },
            '-' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_DASH_ASSGN = [2]u8{ '-', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_DASH = '-' });
                    pos += 1;
                }
            },
            '*' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_ASTERISK_ASSGN = [2]u8{ '*', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_ASTERISK = '*' });
                    pos += 1;
                }
            },
            '/' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_SLASH_ASSGN = [2]u8{ '/', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_SLASH = '/' });
                    pos += 1;
                }
            },
            '%' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_PCNTG_ASSGN = [2]u8{ '%', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_PCNTG = '%' });
                    pos += 1;
                }
            },
            '=' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_EQUALS = [2]u8{ '=', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_ASSIGN = '=' });
                    pos += 1;
                }
            },

            // Conditional Operators
            '!' => {
                try tokens.append(TokenType{ .OP_EXCLMTN = '!' });
                pos += 1;
            },
            '>' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_GTE = [2]u8{ '>', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_GT = '>' });
                    pos += 1;
                }
            },
            '<' => {
                if (peek(source, pos + 1) == '=') {
                    try tokens.append(TokenType{ .OP_LTE = [2]u8{ '<', '=' } });
                    pos += 2;
                } else {
                    try tokens.append(TokenType{ .OP_LT = '<' });
                    pos += 1;
                }
            },

            // Parenthesis
            '(' => {
                try tokens.append(TokenType{ .SYM_LPAREN = '(' });
                pos += 1;
            },
            ')' => {
                try tokens.append(TokenType{ .SYM_RPAREN = ')' });
                pos += 1;
            },
            '[' => {
                try tokens.append(TokenType{ .SYM_LBRACKET = '[' });
                pos += 1;
            },
            ']' => {
                try tokens.append(TokenType{ .SYM_RBRACKET = ']' });
                pos += 1;
            },
            '{' => {
                try tokens.append(TokenType{ .SYM_LBRACE = '{' });
                pos += 1;
            },
            '}' => {
                try tokens.append(TokenType{ .SYM_RBRACE = '}' });
                pos += 1;
            },

            // Eat whitespaces
            ' ', '\n' => {
                pos += 1;
            },
            else => {
                // LITERAL STRING
                if (current == '"' or current == '\'') {
                    var delimiters = [1]u8{current};
                    const literal_string = try fetchStringUntil(allocator, source[pos + 1 ..], &delimiters, false);
                    try tokens.append(TokenType{ .LIT_STRING = literal_string });
                    pos += literal_string.items.len + 2;
                    continue;
                }

                // LITERAL NUMBER
                if (std.ascii.isDigit(current)) {
                    const literal_number = try fetchNumericUntil(allocator, source[pos..]);
                    defer literal_number.deinit();
                    const float_value = try std.fmt.parseFloat(f64, literal_number.items);
                    try tokens.append(TokenType{ .LIT_NUMBER = float_value });
                    pos += literal_number.items.len;
                    continue;
                }

                // IDENT
                if (std.ascii.isAlphabetic(current)) {
                    // Fetch all following characters until whitespace
                    var delimiters = [2]u8{ ' ', '\n' };
                    const identifier = try fetchStringUntil(allocator, source[pos..], &delimiters, true);
                    try tokens.append(TokenType{ .IDENT = identifier });
                    pos += identifier.items.len;
                    continue;
                }

                return error.InvalidParseValue;
            },
        }
    }
    return tokens;
}

fn fetchStringUntil(allocator: std.mem.Allocator, source: []const u8, limits: []u8, allow_eof: bool) !std.ArrayList(u8) {
    var string = std.ArrayList(u8).init(allocator);
    var pos: usize = 0;

    errdefer string.deinit();

    while (true) {
        if (pos >= source.len and !allow_eof) return error.InvalidParseValue;
        if (pos >= source.len and allow_eof) break;

        var shouldStop = false;
        for (limits) |limit| {
            if (source[pos] == limit) {
                shouldStop = true;
                break;
            }
        }
        if (shouldStop) break;

        try string.append(source[pos]);
        pos += 1;
    }
    return string;
}

fn fetchNumericUntil(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(u8) {
    var number = std.ArrayList(u8).init(allocator);
    errdefer number.deinit();
    var pos: usize = 0;
    while (true) {
        if (pos >= source.len or source[pos] == ' ' or source[pos] == '\n') break;
        if (!std.ascii.isDigit(source[pos]) and source[pos] != '.') return error.InvalidParseValue;
        try number.append(source[pos]);
        pos += 1;
    }
    return number;
}

fn peek(source: []const u8, pos: usize) u8 {
    if (pos >= source.len) return 0;
    return source[pos];
}

fn debugTokenPrinter(tokens: std.ArrayList(TokenType)) void {
    std.debug.print("\n", .{});
    for (tokens.items) |token| {
        switch (token) {
            .IDENT => std.debug.print("IDENT: {s}", .{token.IDENT.items}),
            .LIT_NUMBER => std.debug.print("LIT_NUMBER: {d}", .{token.LIT_NUMBER}),
            .LIT_STRING => std.debug.print("LIT_STRING: {s}", .{token.LIT_STRING.items}),

            .OP_PLUS => std.debug.print("OP_PLUS", .{}),
            .OP_DASH => std.debug.print("OP_DASH", .{}),
            .OP_ASTERISK => std.debug.print("OP_ASTERISK", .{}),
            .OP_SLASH => std.debug.print("OP_SLASH", .{}),
            .OP_PCNTG => std.debug.print("OP_PCNTG", .{}),
            .OP_ASSIGN => std.debug.print("OP_ASSIGN", .{}),
            .OP_PLUS_ASSGN => std.debug.print("OP_PLUS_ASSGN", .{}),
            .OP_DASH_ASSGN => std.debug.print("OP_DASH_ASSGN", .{}),
            .OP_ASTERISK_ASSGN => std.debug.print("OP_ASTERISK_ASSGN", .{}),
            .OP_SLASH_ASSGN => std.debug.print("OP_SLASH_ASSGN", .{}),
            .OP_PCNTG_ASSGN => std.debug.print("OP_PCNTG_ASSGN", .{}),
            .OP_LT => std.debug.print("OP_LT", .{}),
            .OP_LTE => std.debug.print("OP_LTE", .{}),
            .OP_GT => std.debug.print("OP_GT", .{}),
            .OP_GTE => std.debug.print("OP_GTE", .{}),
            .OP_EXCLMTN => std.debug.print("OP_EXCLMTN", .{}),
            .OP_EQUALS => std.debug.print("OP_EQUALS", .{}),

            .SYM_LBRACE => std.debug.print("SYM_LBRACE", .{}),
            .SYM_RBRACE => std.debug.print("SYM_RBRACE", .{}),
            .SYM_LPAREN => std.debug.print("SYM_LPAREN", .{}),
            .SYM_RPAREN => std.debug.print("SYM_RPAREN", .{}),
            .SYM_LBRACKET => std.debug.print("SYM_LBRACKET", .{}),
            .SYM_RBRACKET => std.debug.print("SYM_RBRACKET", .{}),
            .SYM_COLON => std.debug.print("SYM_COLON", .{}),
        }
        std.debug.print("\n", .{});
    }
}

test "Lexer lexes operators correctly" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var tokens = try lex(gpa.allocator(), "+ - * / % = += -= *= /= %= == < <= > >= !");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 17);
    try std.testing.expect(tokens.items[0].OP_PLUS == '+');
    try std.testing.expect(tokens.items[1].OP_DASH == '-');
    try std.testing.expect(tokens.items[2].OP_ASTERISK == '*');
    try std.testing.expect(tokens.items[3].OP_SLASH == '/');
    try std.testing.expect(tokens.items[4].OP_PCNTG == '%');
    try std.testing.expect(tokens.items[5].OP_ASSIGN == '=');
    try std.testing.expect(tokens.items[6].OP_PLUS_ASSGN[0] == '+' and tokens.items[6].OP_PLUS_ASSGN[1] == '=');
    try std.testing.expect(tokens.items[7].OP_DASH_ASSGN[0] == '-' and tokens.items[7].OP_DASH_ASSGN[1] == '=');
    try std.testing.expect(tokens.items[8].OP_ASTERISK_ASSGN[0] == '*' and tokens.items[8].OP_ASTERISK_ASSGN[1] == '=');
    try std.testing.expect(tokens.items[9].OP_SLASH_ASSGN[0] == '/' and tokens.items[9].OP_SLASH_ASSGN[1] == '=');
    try std.testing.expect(tokens.items[10].OP_PCNTG_ASSGN[0] == '%' and tokens.items[10].OP_PCNTG_ASSGN[1] == '=');
    try std.testing.expect(tokens.items[11].OP_EQUALS[0] == '=' and tokens.items[11].OP_EQUALS[1] == '=');
    try std.testing.expect(tokens.items[12].OP_LT == '<');
    try std.testing.expect(tokens.items[13].OP_LTE[0] == '<' and tokens.items[13].OP_LTE[1] == '=');
    try std.testing.expect(tokens.items[14].OP_GT == '>');
    try std.testing.expect(tokens.items[15].OP_GTE[0] == '>' and tokens.items[15].OP_GTE[1] == '=');
    try std.testing.expect(tokens.items[16].OP_EXCLMTN == '!');
}

test "Lexer lexes identifiers correctly" {
    const allocator = std.testing.allocator;
    var tokens = try lex(allocator, "hello world test");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 3);
    try std.testing.expect(std.mem.eql(u8, tokens.items[0].IDENT.items, "hello"));
    try std.testing.expect(std.mem.eql(u8, tokens.items[1].IDENT.items, "world"));
    try std.testing.expect(std.mem.eql(u8, tokens.items[2].IDENT.items, "test"));
}

test "Lexer lexes strings correctly" {
    const allocator = std.testing.allocator;
    var tokens = try lex(allocator, "'hello world' \"hello world2\"");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 2);
    try std.testing.expect(std.mem.eql(u8, tokens.items[0].LIT_STRING.items, "hello world"));
    try std.testing.expect(std.mem.eql(u8, tokens.items[1].LIT_STRING.items, "hello world2"));
}

test "Lexer lexes integers correctly" {
    const allocator = std.testing.allocator;
    var tokens = try lex(allocator, "123 456 789");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 3);
    try std.testing.expect(tokens.items[0].LIT_NUMBER == 123);
    try std.testing.expect(tokens.items[1].LIT_NUMBER == 456);
    try std.testing.expect(tokens.items[2].LIT_NUMBER == 789);
}

test "Lexer lexes floats correctly" {
    const allocator = std.testing.allocator;
    var tokens = try lex(allocator, "123.456 789.012");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 2);
    try std.testing.expect(tokens.items[0].LIT_NUMBER == 123.456);
    try std.testing.expect(tokens.items[1].LIT_NUMBER == 789.012);
}

test "Lexer lexes parenthesis correctly" {
    const allocator = std.testing.allocator;
    var tokens = try lex(allocator, "() [] {}");
    defer destroyTokens(&tokens);
    debugTokenPrinter(tokens);
    try std.testing.expect(tokens.items.len == 6);
    try std.testing.expect(tokens.items[0].SYM_LPAREN == '(');
    try std.testing.expect(tokens.items[1].SYM_RPAREN == ')');
    try std.testing.expect(tokens.items[2].SYM_LBRACKET == '[');
    try std.testing.expect(tokens.items[3].SYM_RBRACKET == ']');
    try std.testing.expect(tokens.items[4].SYM_LBRACE == '{');
    try std.testing.expect(tokens.items[5].SYM_RBRACE == '}');
}

test "Lexer lexes broken string literals" {
    const allocator = std.testing.allocator;
    const result = lex(allocator, "'asddff");
    try std.testing.expectError(error.InvalidParseValue, result);
}
