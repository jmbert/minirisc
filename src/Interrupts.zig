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

    ECALL_M = 11,
    INSTR_PFLT = 12,
    LD_PFLT = 13,

    STR_PFLT = 14,
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

    data: ?u64,

    level: PrivMode, // TODO - make this optional. This shouldnt be used before execution

    pub fn New(code: u6, isException: bool, data: u64) !Interrupt {
        if (isException) {
            return Interrupt{
                .n = .{ .EXCEPTION = @enumFromInt(code) },
                .level = .MACHINE,

                .data = data,
            };
        } else {
            return Interrupt{
                .n = .{ .INTERRUPT = @enumFromInt(code) },
                .level = .MACHINE,
                .data = null,
            };
        }
    }

    pub fn Service(self: *Interrupt, cpu: *Cpu, level: PrivMode) !void {
        self.level = level;
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

        var mtvalHandle = cpu.registers.CSRegisterHandle(.MTVAL);
        if (self.data) |data| {
            mtvalHandle.Write(data);
        } else {
            // TODO - Log an error here, or something
            mtvalHandle.Write(0);
        }

        var mepcHandle = cpu.registers.CSRegisterHandle(.MEPC);
        var mtvecHandle = cpu.registers.CSRegisterHandle(.MTVEC);
        var pcHandle = cpu.registers.PCHandle();

        mepcHandle.Write((pcHandle.Read() - 1) & ~@as(u64, 0b11));
        pcHandle.Write(mtvecHandle.Read() & ~@as(Register, @intCast(0x3)));
    }

    fn ServiceInterrupt(self: *Interrupt, cpu: *Cpu) !void {
        _ = cpu;
        switch (self.level) {
            else => return InterruptError.UnimplementedInterruptLevel,
        }
    }
};
