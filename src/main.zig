const std = @import("std");
const Lexer = @import("./lexer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const tokens = try Lexer.lex(gpa.allocator(), "hello world");
    std.debug.print("Tokens: {d}", .{tokens.items.len});
}
