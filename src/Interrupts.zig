const std = @import("std");
const Cpu = @import("Cpu.zig");
const RegisterFile = @import("RegisterFile.zig");
const Register = RegisterFile.Register;
const VirtualAddress = @import("Memory.zig").VirtualAddress;
const PrivMode = Cpu.PrivMode;

pub const ePriorities = [_]ExceptionCode{
    .BRK,

    .INSTR_ACCESS_FLT,

    .ILLEGAL_INSTR,

    .INSTR_ADDR_MA,

    .ECALL_M,
    .ECALL_S,
    .ECALL_U,

    .BRK,
    .BRK,
};

pub const ExceptionCode = enum(u6) {
    INSTR_ADDR_MA = 0,
    INSTR_ACCESS_FLT = 1,
    ILLEGAL_INSTR = 2,
    BRK = 3,
    LD_ADDR_M = 4,
    LD_ACCESS_FLT = 5,
    STR_ADDR_MA = 6,
    STR_ACCESS_FLT = 7,
    ECALL_U = 8,
    ECALL_S = 9,

    ECALL_M = 10,
    INSTR_PFLT = 11,
    LD_PFLT = 12,

    STR_PFLT = 13,
};

pub const InterruptError = error{
    UnknownVectorMode,
    UnimplementedInterruptLevel,
};

pub const InterruptCode = enum(u6) {
    // TODO - Actual interrupts
};

pub const Interrupt = struct {
    n: union(enum) {
        EXCEPTION: ExceptionCode,
        INTERRUPT: InterruptCode,
    },

    level: PrivMode,

    pub fn New(code: u6, isException: bool, level: PrivMode) !Interrupt {
        if (isException) {
            return Interrupt{
                .n = .{ .EXCEPTION = @enumFromInt(code) },
                .level = level,
            };
        } else {
            return Interrupt{
                .n = .{ .INTERRUPT = @enumFromInt(code) },
                .level = level,
            };
        }
    }

    pub fn Service(self: *Interrupt, cpu: *Cpu) !void {
        switch (self.n) {
            .EXCEPTION => return self.ServiceException(cpu),
            .INTERRUPT => return self.ServiceInterrupt(cpu),
        }
    }

    fn ServiceException(self: *Interrupt, cpu: *Cpu) !void {
        switch (self.level) {
            .MACHINE => return self.ServiceMachineException(cpu),
            else => return InterruptError.UnimplementedInterruptLevel,
        }
    }

    fn ServiceMachineException(self: *Interrupt, cpu: *Cpu) !void {
        var code = self.n.EXCEPTION;
        var mcauseHandle = cpu.registers.CSRegisterHandle(.MCAUSE);
        mcauseHandle.Write(mcauseHandle.Read() | @intFromEnum(code));
        // Wrote MCAUSE

        var mstatusHandle = cpu.registers.CSRegisterHandle(.MSTATUS);

        // Transfer MIE to MPIE
        var mie: u1 = @truncate((mstatusHandle.Read() & 0b1_0_0) >> 2); // Get MIE bit

        var maskedmstatus = (mstatusHandle.Read() & ~@as(Register, @intCast(1 << 7)));
        mstatusHandle.Write(maskedmstatus | @as(Register, @intCast(mie)) << 7); // Set MPIE bit

        var mpp: u2 = @intFromEnum(cpu.privilegeMode);

        var maskedmstatus_mpp = (mstatusHandle.Read() & ~@as(Register, @intCast(3 << 11)));
        mstatusHandle.Write(maskedmstatus_mpp | @as(Register, @intCast(mpp)) << 11); // Set MPP bits

        cpu.privilegeMode = .MACHINE; // Machine exception

        // TODO - Set MTVAL here

        var mepcHandle = cpu.registers.CSRegisterHandle(.MEPC);
        var mtvecHandle = cpu.registers.CSRegisterHandle(.MTVEC);
        var pcHandle = cpu.registers.PCHandle();

        mepcHandle.Write(pcHandle.Read());

        pcHandle.Write(mtvecHandle.Read() & ~@as(Register, @intCast(0x3)));
    }

    fn ServiceInterrupt(self: *Interrupt, cpu: *Cpu) !void {
        _ = cpu;
        switch (self.level) {
            else => return InterruptError.UnimplementedInterruptLevel,
        }
    }
};
