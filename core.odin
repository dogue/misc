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
    LDA, LDX, LDY, LDZ, LDW,
    ADD, SUB, MUL, HLT,
    AND, OR, XOR, NOT,
    JLT, JGT, JLE, JGE, JEQ, JNE,
    PSH, POP,
    ROL, ROR,
    ASL, ASR,
    CAL, RET, JMP,
    STO,
    INC, DEC,
}

Opcode :: bit_field u8 {
    inst: Instruction   | 5,
    mode: AddrMode | 3,
}

RegisterByte :: bit_field u8 {
    reg_low: u8      | 2,
    reg_high: u8 | 2,
    quick: u8     | 4,
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

fetch :: proc(c: ^Core) -> u8 {
    b := c.mem[c.pc]
    c.pc += 1
    return b
}

fetch_register :: proc(c: ^Core) -> u8 {
    reg_byte := RegisterByte(fetch(c))
    return c.reg[reg_byte.reg_low]
}

fetch_zeropage :: proc(c: ^Core) -> u8 {
    al := fetch(c)
    return mem_read(c, al, 0x00)
}

fetch_absolute :: proc(c: ^Core) -> u8 {
    al := fetch(c)
    ah := fetch(c)
    return mem_read(c, al, ah)
}

fetch_indexed_zeropage :: proc(c: ^Core) -> u8 {
    al := fetch(c)
    al += c.reg.x
    return mem_read(c, al, 0x00)
}

fetch_indexed_absolute :: proc(c: ^Core) -> u8 {
    al := fetch(c)
    ah := fetch(c)
    addr := u16(ah << 8) | u16(al)
    addr += u16(c.reg.x)
    return mem_read(c, addr)
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
    op: Opcode = Opcode(fetch(c))

    #partial switch op.inst {
    case .NOP: return
    case .LDA: lda(c, op.mode)
    case .LDX: ldx(c, op.mode)
    case .LDY: ldy(c, op.mode)
    case .LDZ: ldz(c, op.mode)
    case .LDW: ldw(c, op.mode)
    case .ADD:
    case .SUB:
    case .MUL:
    case .HLT: c.halted = true
    case .AND:
    case .OR:
    case .XOR:
    case .NOT:
    case .JLT:
    case .JGT:
    case .JLE:
    case .JGE:
    case .JEQ:
    case .JNE:
    case .PSH:
    case .POP:
    case .ROL:
    case .ROR:
    case .ASL:
    case .ASR:
    case .CAL:
    case .RET:
    case .JMP:
    case .STO:
    case .INC:
    case .DEC:
    }
}

lda :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.acc = operand
    c.status.zero = c.acc == 0
}

ldx :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Implied:         operand = c.acc
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.reg.x = operand
    c.status.zero = c.reg.x == 0
}

ldy :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Implied:         operand = c.acc
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.reg.y = operand
    c.status.zero = c.reg.y == 0
}

ldz :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Implied:         operand = c.acc
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.reg.z = operand
    c.status.zero = c.reg.z == 0
}

ldw :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Implied:         operand = c.acc
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.reg.w = operand
    c.status.zero = c.reg.w == 0
}

add :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    result1, carry1 := bits.overflowing_add(c.acc, operand)
    result2, carry2 := bits.overflowing_add(result1, u8(c.status.carry))

    c.acc = result2
    c.status.carry = carry1 || carry2
    c.status.zero = c.acc == 0
}

sub :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    result1, borrow1 := bits.overflowing_sub(c.acc, operand)
    result2, borrow2 := bits.overflowing_sub(result1, u8(!c.status.carry))

    c.acc = result2
    c.status.carry = !borrow1 || !borrow2
    c.status.zero = c.acc == 0
}

mul :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.acc *= operand
    c.status.zero = c.acc == 0
}

and :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.acc &= operand
    c.status.zero = c.acc == 0
}

or :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.acc |= operand
    c.status.zero = c.acc == 0
}

xor :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Immediate:       operand = fetch(c)
    case .Register:        operand = fetch_register(c)
    case .Absolute:        operand = fetch_absolute(c)
    case .ZeroPage:        operand = fetch_zeropage(c)
    case .IndexedAbsolute: operand = fetch_indexed_absolute(c)
    case .IndexedZeroPage: operand = fetch_indexed_zeropage(c)

    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    c.acc ~= operand
    c.status.zero = c.acc == 0
}

// not - to be replaced

jlt :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if !c.status.carry {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if !c.status.carry {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

jgt :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if c.status.carry && !c.status.zero {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if c.status.carry && !c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

jle :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if !c.status.carry || c.status.zero {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if !c.status.carry || c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

jge :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if c.status.carry {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if c.status.carry {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

jeq :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if c.status.zero {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

jne :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)

        if !c.status.zero {
            c.pc = target
        }

    case .Relative:
        offset := i8(fetch(c))
        if !c.status.zero {
            c.pc = u16(i16(c.pc) + i16(offset))
        }

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

psh :: proc(c: ^Core, mode: AddrMode) {
    operand: u8

    switch mode {
    case .Implied:   operand = c.acc
    case .Immediate: operand = fetch(c)
    case .Register:  operand = fetch_register(c)

    case .Absolute: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }

    push_stack(c, operand)
}

pop :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Implied:
        c.acc = pop_stack(c)

    case .Register:
        reg_byte: RegisterByte = RegisterByte(fetch(c))
        c.reg[reg_byte.reg_low] = pop_stack(c)

    case .Immediate: fallthrough
    case .Absolute: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
        return
    }
}

asl :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Implied:
        c.status.carry = bool(c.acc & 0x80)
        c.acc <<= 1
        c.status.zero = c.acc == 0

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .Absolute: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
    }
}

asr :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Implied:
        c.status.carry = bool(c.acc & 0x01)
        c.acc >>= 1
        c.status.zero = c.acc == 0

    case .Immediate: fallthrough
    case .Register: fallthrough
    case .Absolute: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
    }
}

jmp :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        target := u16(ah << 8) | u16(al)
        c.pc = target

    case .Relative:
        offset := i8(fetch(c))
        c.pc = u16(i16(c.pc) + i16(offset))

    case .Register:
        reg_byte := RegisterByte(fetch(c))
        al := reg_byte.reg_low
        ah := reg_byte.reg_high
        target := u16(ah << 8) | u16(al)
        c.pc = target

    case .Immediate: fallthrough
    case .ZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .IndexedZeroPage: fallthrough
    case .Implied:
        raise(c, .InvalidAddrMode)
    }
}

sta :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        mem_write(c, al, ah, c.acc)

    case .ZeroPage:
        al := fetch(c)
        mem_write(c, al, 0x00, c.acc)

    case .IndexedAbsolute:
        al := fetch(c)
        ah := fetch(c)
        addr := u16(ah << 8) | u16(al)
        addr += u16(c.reg.x)
        mem_write(c, addr, c.acc)

    case .IndexedZeroPage:
        al := fetch(c)
        al += c.reg.x
        mem_write(c, al, 0x00, c.acc)

    case .Register:
        reg_byte := RegisterByte(fetch(c))
        mem_write(c, reg_byte.reg_low, reg_byte.reg_high, c.acc)

    case .Immediate: fallthrough
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
    }
}

str :: proc(c: ^Core, mode: AddrMode) {
    reg_byte := RegisterByte(fetch(c))
    data := c.reg[reg_byte.reg_low]

    switch mode {
    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        mem_write(c, al, ah, data)

    case .ZeroPage:
        al := fetch(c)
        mem_write(c, al, 0x00, data)

    case .IndexedZeroPage: fallthrough
    case .IndexedAbsolute: fallthrough
    case .Register: fallthrough
    case .Immediate: fallthrough
    case .Implied: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)

    }
}

inc :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Implied:
        c.acc, c.status.carry = bits.overflowing_add(c.acc, 1)
        c.status.zero = c.acc == 0

    case .Register:
        reg_byte := RegisterByte(fetch(c))
        delta := reg_byte.quick
        if delta == 0 { delta = 1 } // use quick const if present, else 1
        c.reg[reg_byte.reg_low], c.status.carry = bits.overflowing_add(c.reg[reg_byte.reg_low], delta)
        c.status.zero = c.reg[reg_byte.reg_low] == 0

    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        val := mem_read(c, al, ah)
        mem_write(c, al, ah, val + 1)

    case .ZeroPage:
        al := fetch(c)
        val := mem_read(c, al, 0x00)
        mem_write(c, al, 0x00, val + 1)

    case .IndexedAbsolute:
        al := fetch(c)
        ah := fetch(c)
        addr := u16(ah << 8) | u16(al)
        addr += u16(c.reg.x)
        val := mem_read(c, addr)
        mem_write(c, addr, val + 1)

    case .IndexedZeroPage:
        al := fetch(c)
        al += c.reg.x
        val := mem_read(c, al, 0x00)
        mem_write(c, al, 0x00, val + 1)

    case .Immediate: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
    }
}

dec :: proc(c: ^Core, mode: AddrMode) {
    switch mode {
    case .Implied:
        c.acc, c.status.carry = bits.overflowing_sub(c.acc, 1)
        c.status.zero = c.acc == 0

    case .Register:
        reg_byte := RegisterByte(fetch(c))
        delta := reg_byte.quick
        if delta == 0 { delta = 1 } // use quick const if present, else 1
        c.reg[reg_byte.reg_low], c.status.carry = bits.overflowing_sub(c.reg[reg_byte.reg_low], delta)
        c.status.zero = c.reg[reg_byte.reg_low] == 0

    case .Absolute:
        al := fetch(c)
        ah := fetch(c)
        val := mem_read(c, al, ah)
        mem_write(c, al, ah, val - 1)

    case .ZeroPage:
        al := fetch(c)
        val := mem_read(c, al, 0x00)
        mem_write(c, al, 0x00, val - 1)

    case .IndexedAbsolute:
        al := fetch(c)
        ah := fetch(c)
        addr := u16(ah << 8) | u16(al)
        addr += u16(c.reg.x)
        val := mem_read(c, addr)
        mem_write(c, addr, val - 1)

    case .IndexedZeroPage:
        al := fetch(c)
        al += c.reg.x
        val := mem_read(c, al, 0x00)
        mem_write(c, al, 0x00, val - 1)

    case .Immediate: fallthrough
    case .Relative:
        raise(c, .InvalidAddrMode)
    }
}
