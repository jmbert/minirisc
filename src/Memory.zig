const std = @import("std");
const defs = @import("defs.zig");
const Byte = defs.Byte;
const Allocator = std.mem.Allocator;

const Memory = @This();

pub const PhysicalAddress = u64;
pub const VirtualAddress = u64;
pub const MemorySegmentArray = std.ArrayList(*MemorySegment);

pub const MemAccessError: type = error{
    NoSegmentMapped,
    AddressTooLarge,
};

pub const MemorySegment: type = struct {
    base: u64,
    limit: u64,
    id: [:0]u8,

    userData: *anyopaque,

    callback: *const fn (
        self: *MemorySegment,
        address: PhysicalAddress,
        data: ?Byte,
    ) MemAccessError!?Byte,

    pub fn format(
        value: MemorySegment,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        return writer.print("{s}: {X}-{X}", .{ value.id, value.base, value.base + value.limit });
    }
};

segments: MemorySegmentArray,

fn getSegment(self: *Memory, addr: PhysicalAddress) ?*MemorySegment {
    for (self.segments.items) |segment| {
        var base = segment.base;
        var limit = segment.limit;
        if (addr >= base and addr <= base + limit) {
            return segment;
        }
    }
    return null;
}

fn readByte(self: *Memory, addr: PhysicalAddress) MemAccessError!Byte {
    var segment = self.getSegment(addr);
    if (segment) |seg| {
        var byte = try seg.callback(seg, addr - seg.base, null);
        // seg.callback is forbidden to return no error and a null pointer
        // when byte is null
        return byte.?;
    } else {
        return MemAccessError.NoSegmentMapped;
    }
}

fn writeByte(self: *Memory, addr: PhysicalAddress, data: Byte) MemAccessError!void {
    var segment = self.getSegment(addr);
    if (segment) |seg| {
        // Discard return value
        _ = try seg.callback(seg, addr - seg.base, data);
    } else {
        return MemAccessError.NoSegmentMapped;
    }
}

pub fn Read(self: *Memory, addr: VirtualAddress, buffer: []Byte) MemAccessError!void {
    var size = buffer.len;
    for (0..size, addr..) |byte_index, byte_addr| {
        var byte: Byte = try self.readByte(byte_addr);
        buffer[byte_index] = byte;
    }
}

pub fn Write(self: *Memory, addr: VirtualAddress, buffer: []Byte) MemAccessError!void {
    var size = buffer.len;
    for (0..size, addr..) |byte_index, byte_addr| {
        try self.writeByte(byte_addr, buffer[byte_index]);
    }
}

pub fn New(allocator: Allocator) !*Memory {
    var m: *Memory = try allocator.create(Memory);
    m.* = Memory{
        .segments = try MemorySegmentArray.initCapacity(allocator, 1),
    };

    return m;
}

pub fn AddSegment(self: *Memory, seg: *MemorySegment) !void {
    return self.segments.append(seg);
}

pub fn format(
    value: Memory,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) std.os.WriteError!void {
    for (value.segments.items) |seg| {
        try writer.print("{any}\n", .{seg});
    }
}
