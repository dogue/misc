package misc

import "core:math/bits"

Exception :: enum u8 {
    Nil,
    InvalidAddrMode,
}

Status :: bit_field u8 {
    carry: bool          | 1,
    zero: bool           | 1,
    reserved: u8         | 2,
    exception: Exception | 4,
}

AddrMode :: enum u8 {
    Implied,
    Immediate,
    Register,
    Absolute,
    ZeroPage,
    IndexedZeroPage,
    IndexedAbsolute,
    Relative,
}

Instruction :: enum u8 {
    NOP,
    LDA, LDR,
    CLR, SWP, CMP,
    ADD, SUB, MUL, HLT,
    AND, OR, XOR, STA,
    JLT, JGT, JLE, JGE, JEQ, JNE,
    PSH, POP,
    ASL, ASR,
    JMP,
    STR,
    INC, DEC,
}

Opcode :: bit_field u8 {
    inst: Instruction   | 5,
    mode: AddrMode | 3,
}

RegisterByte :: bit_field u8 {
    reg: u8   | 2,
    extra: u8 | 6,
}

Core :: struct {
    reg:    [4]byte,
    acc:    byte,
    sp:     byte,
    pc:     u16,
    status: Status,
    halted: bool,
    mem:    [65535]byte,
}

init :: proc() -> Core {
    core := Core{}
    core.mem = [65535]byte{0..<65535 = 0}
    return core
}

reset :: proc(c: ^Core) {
    c.reg    = {0..<4 = 0}
    c.acc    = 0
    c.sp     = 0
    c.pc     = 0xFF00
    c.status = Status(0)
}

push_stack :: proc(c: ^Core, value: u8) {
    c.sp -= 1
    mem_write(c, c.sp, 0x3f, value)
}

pop_stack :: proc(c: ^Core) -> u8 {
    value := mem_read(c, c.sp, 0x3f)
    c.sp += 1
    return value
}

fetch_byte :: proc(c: ^Core, mode: AddrMode = .Immediate) -> (data: u8) {
    #partial switch mode {
    case .Immediate:
        data = mem_read(c, c.pc)
        c.pc += 1

    case .Register:
        rb := RegisterByte(fetch_byte(c))
        data = c.reg[rb.reg]

    case .Absolute:
        al := fetch_byte(c)
        ah := fetch_byte(c)
        data = mem_read(c, al, ah)

    case .ZeroPage:
        al := fetch_byte(c)
        data = mem_read(c, al, 0x00)

    case .IndexedAbsolute:
        al := fetch_byte(c)
        ah := fetch_byte(c)
        addr := u16(ah << 8) | u16(al)
        addr += u16(c.reg.x)
        data = mem_read(c, addr)

    case .IndexedZeroPage:
        al := fetch_byte(c)
        al += c.reg.x
        data = mem_read(c, al, 0x00)

    // shouldn't happen? *shrug*
    case:
        data = 0
    }

    return
}

fetch_addr :: proc(c: ^Core, mode: AddrMode) -> (addr: u16) {
    #partial switch mode {
    case .Absolute:
        al := fetch_byte(c)
        ah := fetch_byte(c)
        addr = u16(ah << 8) | u16(al)

    case .ZeroPage:
        al := fetch_byte(c)
        addr = u16(al)

    case .IndexedAbsolute:
        al := fetch_byte(c)
        ah := fetch_byte(c)
        addr = u16(ah << 8) | u16(al)
        addr += u16(c.reg.x)

    case .IndexedZeroPage:
        al := fetch_byte(c)
        al += c.reg.x
        addr = u16(al)

    case .Register:
        rb := RegisterByte(fetch_byte(c))
        addr = u16(rb.extra << 8) | u16(rb.reg)
    }

    return
}

mem_read :: proc {
    mem_read_addr,
    mem_read_low_high,
}

mem_read_addr :: proc(c: ^Core, addr: u16) -> u8 {
    return c.mem[addr]
}

mem_read_low_high :: proc(c: ^Core, al, ah: u8) -> u8 {
    addr := u16(ah << 8) | u16(al)
    return mem_read(c, addr)
}

mem_write :: proc {
    mem_write_addr,
    mem_write_low_high,
}

mem_write_addr :: proc(c: ^Core, addr: u16, data: u8) {
    c.mem[addr] = data
}

mem_write_low_high :: proc(c: ^Core, al, ah: u8, data: u8) {
    addr := u16(ah << 8) | u16(al)
    mem_write_addr(c, addr, data)
}

raise :: proc(c: ^Core, e: Exception) {
    c.status.exception = e
    c.halted = true
}

tick :: proc(c: ^Core) {
    op: Opcode = Opcode(fetch_byte(c))

    switch op.inst {
    case .NOP: return
    case .LDA: lda(c, op.mode)
    case .LDR: ldr(c, op.mode)
    case .CLR:
    case .SWP:
    case .CMP:
    case .ADD: add(c, op.mode)
    case .SUB: sub(c, op.mode)
    case .MUL: mul(c, op.mode)
    case .HLT: c.halted = true
    case .AND: and(c, op.mode)
    case .OR:  or(c, op.mode)
    case .XOR: xor(c, op.mode)
    case .STA: sta(c, op.mode)
    case .JLT: jlt(c, op.mode)
    case .JGT: jgt(c, op.mode)
    case .JLE: jle(c, op.mode)
    case .JGE: jge(c, op.mode)
    case .JEQ: jeq(c, op.mode)
    case .JNE: jne(c, op.mode)
    case .PSH: psh(c, op.mode)
    case .POP: pop(c, op.mode)
    // $16
    // $17
    case .ASL: asl(c)
    case .ASR: asr(c)
    // $1A
    // $1B
    case .JMP: jmp(c, op.mode)
    case .STR: str(c, op.mode)
    case .INC: inc(c, op.mode)
    case .DEC: dec(c, op.mode)
    }
}

lda :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)

    case:
        c.acc = fetch_byte(c, mode)
        c.status.zero = c.acc == 0
    }
}

ldr :: proc(c: ^Core, mode: AddrMode) {
    rb := RegisterByte(fetch_byte(c))
    operand: u8

    #partial switch mode {
    case .Relative:
        raise(c, .InvalidAddrMode)

    case .Implied:
        operand = c.acc

    // use extra data in the RegisterByte (small values $00-$3f)
    // to avoid using an extra full byte for the instruction if possible
    case .Immediate:
        if rb.extra == 0 {
            operand = fetch_byte(c)
        } else {
            operand = rb.extra
        }

    case:
        operand = fetch_byte(c, mode)
    }

    c.reg[rb.reg] = operand
    c.status.zero = operand == 0
}

clr :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied:
        c.acc = 0

    case .Register:
        regs := fetch_byte(c)
        if regs & 0x01 != 0 do c.reg.x = 0
        if regs & 0x02 != 0 do c.reg.y = 0
        if regs & 0x04 != 0 do c.reg.z = 0
        if regs & 0x08 != 0 do c.reg.w = 0

    case:
        raise(c, .InvalidAddrMode)
        return
    }

    c.status.zero = true
}

swp :: proc(c: ^Core, mode: AddrMode) {
    rb := RegisterByte(fetch_byte(c))

    #partial switch mode {
    case .Implied:
        c.acc, c.reg[rb.reg] = c.reg[rb.reg], c.acc

    case .Register:
        c.reg[rb.reg], c.reg[rb.extra] = c.reg[rb.extra], c.reg[rb.reg]

    case:
        raise(c, .InvalidAddrMode)
    }
}

cmp :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    result, borrow := bits.overflowing_sub(c.acc, operand)
    c.status.carry = !borrow
    c.status.zero = result == 0
}

add :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    result1, carry1 := bits.overflowing_add(c.acc, operand)
    result2, carry2 := bits.overflowing_add(result1, u8(c.status.carry))

    c.acc = result2
    c.status.carry = carry1 || carry2
    c.status.zero = c.acc == 0
}

sub :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    result1, borrow1 := bits.overflowing_sub(c.acc, operand)
    result2, borrow2 := bits.overflowing_sub(result1, u8(!c.status.carry))

    c.acc = result2
    c.status.carry = !borrow1 || !borrow2
    c.status.zero = c.acc == 0
}

mul :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    c.acc *= operand
    c.status.zero = c.acc == 0
}

and :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    c.acc &= operand
    c.status.zero = c.acc == 0
}

or :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    c.acc |= operand
    c.status.zero = c.acc == 0
}

xor :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    #partial switch mode {
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return

    case:
        operand = fetch_byte(c, mode)
    }

    c.acc ~= operand
    c.status.zero = c.acc == 0
}

sta :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Immediate: fallthrough
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)

    case:
        addr := fetch_addr(c, mode)
        mem_write(c, addr, c.acc)
    }
}

jlt :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if !c.status.carry {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if !c.status.carry {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

jgt :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if c.status.carry && !c.status.zero {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if c.status.carry && !c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

jle :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if !c.status.carry || c.status.zero {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if !c.status.carry || c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

jge :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if c.status.carry {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if c.status.carry {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

jeq :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if c.status.zero {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

jne :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        if !c.status.zero {
            c.pc = fetch_addr(c, .Absolute)
        }

    case .Relative:
        offset := i8(fetch_byte(c))
        if !c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case:
        raise(c, .InvalidAddrMode)
    }
}

psh :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied:   push_stack(c, c.acc)
    case .Immediate: push_stack(c, fetch_byte(c))
    case .Register:  push_stack(c, fetch_byte(c, .Register))

    case:
        raise(c, .InvalidAddrMode)
    }

}

pop :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied:
        c.acc = pop_stack(c)

    case .Register:
        rb: RegisterByte = RegisterByte(fetch_byte(c))
        c.reg[rb.reg] = pop_stack(c)

    case:
        raise(c, .InvalidAddrMode)
    }
}

asl :: proc(c: ^Core) {
    c.status.carry = bool(c.acc & 0x80)
    c.acc <<= 1
    c.status.zero = c.acc == 0
}

asr :: proc(c: ^Core) {
    c.status.carry = bool(c.acc & 0x01)
    c.acc >>= 1
    c.status.zero = c.acc == 0
}

jmp :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Absolute:
        c.pc = fetch_addr(c, .Absolute)

    case .Relative:
        offset := i8(fetch_byte(c))
        c.pc = u16(i16(c.pc) + i16(offset))

    case .Register:
        c.pc = fetch_addr(c, .Register)

    case:
        raise(c, .InvalidAddrMode)
    }
}


str :: proc(c: ^Core, mode: AddrMode) {
    rb := RegisterByte(fetch_byte(c))
    data := c.reg[rb.reg]

    #partial switch mode {
    case .Absolute:
        addr := fetch_addr(c, .Absolute)
        mem_write(c, addr, data)

    case .ZeroPage:
        addr := fetch_addr(c, .ZeroPage)
        mem_write(c, addr, data)

    case:
        raise(c, .InvalidAddrMode)
    }
}

inc :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied:
        c.acc, c.status.carry = bits.overflowing_add(c.acc, 1)
        c.status.zero = c.acc == 0

    case .Register:
        reg_byte := RegisterByte(fetch_byte(c))
        delta := reg_byte.extra
        if delta == 0 { delta = 1 } // use quick const if present, else 1
        c.reg[reg_byte.reg], c.status.carry = bits.overflowing_add(c.reg[reg_byte.reg], delta)
        c.status.zero = c.reg[reg_byte.reg] == 0

    case .Absolute:
        addr := fetch_addr(c, .Absolute)
        val := mem_read(c, addr)
        mem_write(c, addr, val + 1)

    case .ZeroPage:
        addr := fetch_addr(c, .ZeroPage)
        val := mem_read(c, addr)
        mem_write(c, addr, val + 1)

    case .IndexedAbsolute:
        addr := fetch_addr(c, .IndexedAbsolute)
        val := mem_read(c, addr)
        mem_write(c, addr, val + 1)

    case .IndexedZeroPage:
        addr := fetch_addr(c, .IndexedZeroPage)
        val := mem_read(c, addr)
        mem_write(c, addr, val + 1)

    case:
        raise(c, .InvalidAddrMode)
    }
}

dec :: proc(c: ^Core, mode: AddrMode) {
    #partial switch mode {
    case .Implied:
        c.acc, c.status.carry = bits.overflowing_sub(c.acc, 1)
        c.status.zero = c.acc == 0

    case .Register:
        reg_byte := RegisterByte(fetch_byte(c))
        delta := reg_byte.extra
        if delta == 0 { delta = 1 } // use quick const if present, else 1
        c.reg[reg_byte.reg], c.status.carry = bits.overflowing_sub(c.reg[reg_byte.reg], delta)
        c.status.zero = c.reg[reg_byte.reg] == 0

    case .Absolute:
        addr := fetch_addr(c, .Absolute)
        val := mem_read(c, addr)
        mem_write(c, addr, val - 1)

    case .ZeroPage:
        addr := fetch_addr(c, .ZeroPage)
        val := mem_read(c, addr)
        mem_write(c, addr, val - 1)

    case .IndexedAbsolute:
        addr := fetch_addr(c, .IndexedAbsolute)
        val := mem_read(c, addr)
        mem_write(c, addr, val - 1)

    case .IndexedZeroPage:
        addr := fetch_addr(c, .IndexedZeroPage)
        val := mem_read(c, addr)
        mem_write(c, addr, val - 1)

    case:
        raise(c, .InvalidAddrMode)
    }
}
