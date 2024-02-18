const std = @import("std");
const Allocator = std.mem.Allocator;
const mextension = @import("defs.zig").mextension;

const RegisterFile = @This();

pub const RegisterError = error{
    InvalidCSR,
};

pub const xReg = u5;

pub fn writexReg(r: xReg, writer: anytype) std.os.WriteError!void {
    const baseId = "X";
    return writer.print("{s}{d}", .{ baseId, r });
}

pub const csr = enum(u12) {
    MISA = 0x301,
    MHARTID = 0xF14,
    MTVEC = 0x305,
    MEPC = 0x341,
    MIP = 0x344,
    MIE = 0x304,
    MCAUSE = 0x342,
    MSTATUS = 0x300,
    MSCRATCH = 0x340,

    UNIMPLEMENTED = 0xFFF,
    _,
};

pub fn writecsrReg(r: csr, writer: anytype) std.os.WriteError!void {
    switch (r) {
        .MISA, .MHARTID, .MTVEC, .MEPC, .MIP, .MIE, .MCAUSE, .MSTATUS, .MSCRATCH => try writer.print("{s}", .{@tagName(r)}),
        else => try writer.print("CSR_UNKNOWN({X})", .{@intFromEnum(r)}),
    }
}

const mwstatusMask = 0b0_0000000000000000000000000_0_0_00_00_000000000_0_0_0_0_0_0_00_00_11_00_0_1_0_0_0_1_0_0_0;
const mrstatusMask = 0b0_0000000000000000000000000_0_0_00_00_000000000_0_0_0_0_0_0_00_00_11_00_0_1_0_0_0_1_0_0_0;
const mrstatusOMsk = 0b0_0000000000000000000000000_0_0_00_00_000000000_0_0_0_0_0_0_00_00_00_00_0_0_0_0_0_0_0_0_0;

pub const CSRegister = union(csr) {
    MISA: *Register,
    MHARTID: *Register,
    MTVEC: *Register,
    MEPC: *Register,
    MIP: *Register,
    MIE: *Register,
    MCAUSE: *Register,
    MSTATUS: *Register,
    MSCRATCH: *Register,
    UNIMPLEMENTED: Register,

    pub fn Read(self: *CSRegister) Register {
        switch (self.*) {
            .MISA => {
                return @as(u64, mextension) | (2 << 62);
            },
            .MHARTID => return 0,
            .MTVEC, .MEPC, .MIP, .MIE, .MCAUSE, .MSCRATCH => |v| {
                return v.*;
            },
            .MSTATUS => |v| {
                return (v.* & mrstatusMask) | mrstatusMask;
            },
            else => return 0,
        }
    }
    pub fn Write(self: *CSRegister, data: Register) void {
        // TODO - Implement WLRL, WARL
        switch (self.*) {
            .MHARTID, .MISA => return,
            .MTVEC, .MEPC, .MIP, .MIE, .MCAUSE, .MSTATUS => |v| {
                v.* = data & mwstatusMask;
            },
            else => return,
        }
    }

    pub fn New(t: u12, alloc: Allocator) !CSRegister {
        var r = try alloc.create(Register);
        r.* = 0;
        switch (@as(csr, @enumFromInt(t))) {
            .MEPC => return CSRegister{ .MEPC = r },
            .MHARTID => return CSRegister{ .MHARTID = r },
            .MTVEC => return CSRegister{ .MTVEC = r },
            .MISA => return CSRegister{ .MISA = r },
            .MIE => return CSRegister{ .MIE = r },
            .MIP => return CSRegister{ .MIP = r },
            else => return CSRegister{ .UNIMPLEMENTED = 0 },
        }
    }
};

pub const Register = u64;
pub const xRegN = 32;
pub const csrN = 4096;

xRegs: []Register,
pc: Register,
csrs: []CSRegister,

const CSRHandle = struct {
    ptr: *CSRegister,
    indx: csr,

    pub fn Read(self: CSRHandle) Register {
        return self.ptr.Read();
    }
    pub fn Write(self: CSRHandle, data: Register) void {
        return self.ptr.Write(data);
    }
};

const XRHandle = struct {
    ptr: *Register,
    indx: xReg,

    pub fn Read(self: XRHandle) Register {
        switch (self.indx) {
            0 => return 0,
            else => return self.ptr.*,
        }
    }
    pub fn Write(self: XRHandle, data: Register) void {
        switch (self.indx) {
            0 => return,
            else => self.ptr.* = data,
        }
    }
};

const pcHandle = struct {
    pc: *Register,
    pub fn Read(self: pcHandle) Register {
        return self.pc.*;
    }
    pub fn Write(self: pcHandle, data: Register) void {
        self.pc.* = data;
    }
};

pub const RegisterHandle = union(enum) {
    xHandle: XRHandle,
    pcHandle: pcHandle,
    csrHandle: CSRHandle,

    pub fn Read(self: RegisterHandle) Register {
        switch (self) {
            .xHandle => |xr| return xr.Read(),
            .pcHandle => |pc| return pc.Read(),
            .csrHandle => |csrH| return csrH.Read(),
        }
    }

    pub fn Write(self: RegisterHandle, data: Register) void {
        switch (self) {
            .xHandle => |xr| return xr.Write(data),
            .pcHandle => |pc| return pc.Write(data),
            .csrHandle => |csrH| return csrH.Write(data),
        }
    }
};

pub fn XRegisterHandle(self: *RegisterFile, r: xReg) RegisterHandle {
    return RegisterHandle{ .xHandle = XRHandle{
        .ptr = &(self.xRegs[r]),
        .indx = r,
    } };
}

pub fn CSRegisterHandle(self: *RegisterFile, r: csr) RegisterHandle {
    return RegisterHandle{ .csrHandle = CSRHandle{
        .ptr = &(self.csrs[@intFromEnum(r)]),
        .indx = r,
    } };
}
pub fn PCHandle(self: *RegisterFile) RegisterHandle {
    return RegisterHandle{
        .pcHandle = pcHandle{ .pc = &(self.pc) },
    };
}

pub fn New(allocator: Allocator) !*RegisterFile {
    var registers: *RegisterFile = try allocator.create(RegisterFile);
    registers.xRegs = try allocator.alloc(Register, xRegN);
    for (0..xRegN) |i| {
        registers.xRegs[i] = 0;
    }
    registers.csrs = try allocator.alloc(CSRegister, csrN);

    for (0..csrN) |i| {
        registers.csrs[i] = try CSRegister.New(@intCast(i), allocator);
    }

    registers.pc = 0;

    return registers;
}
