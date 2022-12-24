package cp420

import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatestplus.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen


class InstructionDecoderSpec extends AnyFlatSpec 
  with should.Matchers
  with ScalaCheckDrivenPropertyChecks:

  import InstructionDecoder.*,
         AbstractInstruction.*
  import Source as S
  import Target as T
  import Arbitraries.given

  def word(binaryImage: String) = 
    Word.mask(Integer.parseInt(binaryImage, 2))

  "Binary literals" should "work" in {
    b"0000 0000 0000 0000" should be (0)
    b"0101 1010 1111 0000" should be (word("0101101011110000"))
  }

  "Op-codes" should "unmask properly" in {
    OpCode.unmask(b"0101 1010 1111 0000") should be (word("010"))
    OpCode.unmask(b"1011 1010 1111 0000") should be (word("101"))
    OpCode.unmask(b"1111 1010 1111 0000") should be (word("111"))
    OpCode.unmask(b"0001 1010 1111 0000") should be (word("000"))
  }

  "Instruction Decoder" should "decode load immediate byte" in {
    forAll { (x: Int) =>
      val encoded = b"0010 0000 0000 0000" | (x & 0xFF)
      decode(Word.mask(encoded)) should be (Load(S.ImmediateByte(Word.mask(x & 0xFF))))
    }
  }

  it should "decode HCF" in {
    decode(b"0000 0000 0000 0000") should be (HCF)
  }

  it should "decode load immediate word" in {
    forAll { (x: Int) =>
      decodeWide(b"0011 0000 0000 0000", Word.mask(x)) should be (Load(S.ImmediateWord(Word.mask(x))))
    }      
  }

  it should "decode load memory with immediate absolute address" in {
    forAll { (x: Int) =>
      val address = x & 0x00FF_FFFF
      val page = address >>> 16
      val offset = address & 0x0000FFFF
      val encoded = b"0011 0100 0000 0000" | page
      decodeWide(Word.mask(encoded), Word.mask(offset)) should be (Load(S.Memory(Address.mask(address))))
    }
  }

  it should "decode load accumulator from register" in {
    forAll { (register: RegisterName) =>
      val encoded = b"0010 1000 0000 0000" | register.ordinal << 8
      decode(Word.mask(encoded)) should be (Load(S.Register(register)))
    }
  }

  it should "decode a move reg to reg" in {
    forAll { (targetRegister: RegisterName, sourceRegister: RegisterName) =>
      val encoded = 
        b"0010 1000 1000 0000" | targetRegister.ordinal << 8 | sourceRegister.ordinal << 5
      decode(Word.mask(encoded)) should be (Move(targetRegister, sourceRegister))
    }    
  }

  it should "decode load accumulator from register indirect" in {
    forAll { (register: RegisterName, page: Int) =>
      val pageAddress = page & 0x00FF
      val encoded = b"0010 1100 0000 0000" | (register.ordinal << 8) | pageAddress
      decode(Word.mask(encoded)) should be (Load(S.RegisterIndirect(Word.mask(pageAddress), register)))
    }
  }

  it should "decode store accumulator in immediate absolute memory address" in {
    forAll { (x: Int) =>
      val address = x & 0x00FF_FFFF
      val page = address >>> 16
      val offset = address & 0x0000FFFF
      val encoded = b"0101 0100 0000 0000" | page
      decodeWide(Word.mask(encoded), Word.mask(offset)) should be (Store(T.Memory(Address.mask(address))))
    }
  }

  it should "decode store accumulator in register" in {
    forAll { (register: RegisterName) =>
      val encoded = b"0100 1000 0000 0000" | register.ordinal << 8
      decode(Word.mask(encoded)) should be (Store(T.Register(register)))
    }
  }

  it should "decode store accumulator in register indirect" in {
    forAll { (register: RegisterName, page: Int) =>
      val pageAddress = page & 0x00FF
      val encoded = b"0100 1100 0000 0000" | (register.ordinal << 8) | pageAddress
      decode(Word.mask(encoded)) should be (Store(T.RegisterIndirect(Word.mask(pageAddress), register)))
    }
  }

  it should "decode ALU with immediate word" in {
    forAll { (operator: AluOperator, word: Int) =>
      val encoded = b"0111 0000 0000 0000" | operator.opCode.asInt << 8
      val data = Word.mask(word)
      decodeWide(Word.mask(encoded), data) should be (Arithmetic(operator, AluOperand.ImmediateWord(data)))
    }
  }

  it should "decode ALU with a register source" in {
    forAll { (operator: AluOperator, register: RegisterName) =>
      val encoded = b"0110 0000 1000 0000" | (operator.opCode.asInt << 8) | (register.ordinal << 4)
      decode(Word.mask(encoded)) should be (Arithmetic(operator, AluOperand.Register(register)))
    }
  }

  it should "decode SHL/ SHR with immedaite nibble" in {
    forAll { (operator: AluOperator, nibble: Int) =>
      decode(Word.mask(b"0110 1001 0000 0000" | nibble & 0x0F)) should be (Arithmetic(AluOperator.Shl, AluOperand.ImmediateWord(Word.mask(nibble & 0x0F))))
      decode(Word.mask(b"0110 1010 0000 0000" | nibble & 0x0F)) should be (Arithmetic(AluOperator.Shr, AluOperand.ImmediateWord(Word.mask(nibble & 0x0F))))
    }
  }

  it should "decode stack pop to the accumulator" in {
    decode(Word.mask(b"1000 0000 0000 0000")) should be (Pop(PopOperand.Accumulator))
  }

  it should "decode stack pop to a register" in {
    forAll { (register: RegisterName) =>
      val encoded = b"1000 0100 0000 0000" | register.ordinal << 8
      decode(Word.mask(encoded)) should be (Pop(PopOperand.Register(register)))
    }
  }

  it should "decode a stack push from the accumulator" in {
    decode(Word.mask(b"1010 0000 0000 0000")) should be (Push(PushOperand.Accumulator))
  }

  it should "decode a stack push of an immediate byte" in {
    forAll { (byte: Int) =>
      val encoded = b"1010 1000 0000 0000" | byte & 0xFF
      decode(Word.mask(encoded)) should be (Push(PushOperand.ImmediateByte(Word.mask(byte & 0xFF))))
    }
  }

  it should "decode a stack push of an immediate word" in {
    forAll { (word: Int) =>
      val encoded = b"1010 1100 0000 0000"
      val data = word & 0xFFFF
      decodeWide(Word.mask(encoded), Word.mask(data)) should be (Push(PushOperand.ImmediateWord(Word.mask(word & 0xFFFF))))
    }
  }

  it should "decode a jump to absolute address" in {
    forAll { (x: Int) =>
      val address = x & 0x00FF_FFFF
      val page = address >>> 16
      val offset = address & 0x0000FFFF
      val encoded = b"1101 0000 0000 0000" | page

      decodeWide(Word.mask(encoded), Word.mask(offset)) should be (Jump(Address.mask(address)))
    }
  }

  it should "decode a call" in {
    forAll { (x: Int) =>
      val address = x & 0x00FF_FFFF
      val page = address >>> 16
      val offset = address & 0x0000FFFF
      val encoded = b"1101 1000 0000 0000" | page

      decodeWide(Word.mask(encoded), Word.mask(offset)) should be (Call(Address.mask(address)))
    }
  }

  it should "decode a ret" in {
    decode(Word.mask(b"1100 1000 0000 0000")) should be (Return)
  }

  it should "decode a conditional branch" in {
    forAll { (flag: Flag, x: Int, not: Boolean) =>
      val address = x & 0x00FF_FFFF
      val page = address >>> 16
      val offset = address & 0x0000FFFF
      val encoded = b"1101 0000 0000 0000" | flag.code << 8 | page | (if not then 1 else 0) << 11
      val enable = if not then Left(flag) else Right(flag)
      decodeWide(Word.mask(encoded), Word.mask(offset)) should be (Branch(Address.mask(address), enable))
    }
  }

  it should "decode compare with byte" in {
    forAll { (byte: Int) =>
      val encoded = b"1110 0000 0000 0000" | byte & 0xFF
      decode(Word.mask(encoded)) should be (Compare(Source.ImmediateByte(Word.mask(byte & 0xFF))))
    }
  }

  it should "decode compare with a word" in {
    forAll { (word: Int) =>
      val encoded = b"1110 0000 0000 0000"
      val data = Word.mask(word & 0xFFFF)
      decodeWide(Word.mask(encoded), data) should be (Compare(Source.ImmediateWord(data)))
    }
  }

  it should "decode compare with a register" in {
    forAll { (register: RegisterName) =>
      val encoded = b"1110 1000 0000 0000" | register.ordinal << 8
      decode(Word.mask(encoded)) should be (Compare(S.Register(register)))
    }    
  }

  it should "decode compare with a register indirect" in {
    forAll { (register: RegisterName, page: Int) =>
      val pageAddress = page & 0x00FF
      val encoded = b"1110 1100 0000 0000" | (register.ordinal << 8) | pageAddress
      decode(Word.mask(encoded)) should be (Compare(S.RegisterIndirect(Word.mask(pageAddress), register)))
    }
  }