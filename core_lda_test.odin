package misc

import "core:testing"

@(test)
test_lda_implied :: proc(t: ^testing.T) {
    c := init()
    reset(&c)
    
    inst := Opcode {
        mode = .Implied,
        inst = .LDA,
    }

    mem_write(&c, 0xFF00, u8(inst))
    tick(&c)

    testing.expect_value(t, c.status.exception, Exception.InvalidAddrMode)
    testing.expect(t, c.halted)
}


@(test)
test_lda_immediate :: proc(t: ^testing.T) {
    c := init()
    reset(&c)

    inst := Opcode{
        mode = .Immediate,
        inst = .LDA,
    }

    mem_write(&c, 0xFF00, u8(inst))
    mem_write(&c, 0xFF01, 0x69)
    tick(&c)

    testing.expect_value(t, c.acc, 0x69)
}

@(test)
test_lda_register :: proc(t: ^testing.T) {
    c := init()
    reset(&c)

    inst := Opcode {
        mode = .Register,
        inst = .LDA,
    }

    rb := RegisterByte {
        reg = 0,
    }

    mem_write(&c, 0xFF00, u8(inst))
    mem_write(&c, 0xFF01, u8(rb))

    c.reg.x = 0x69
    tick(&c)

    testing.expect_value(t, c.acc, 0x69)
}
