package cp420


/* Instructions are packad into 1 or 2 words, with the
   instruction (and sometimes the data) is in the first word.
   The second word is present for some op-codes.
   A bit in the first Word indicates whether or not it is
   part of a wide instruction.
 */

object InstructionDecoder:
  import AbstractInstruction.*
  final val Wide = 12

  enum OpCode(val code: Int):
    case Halt    extends OpCode(b"000")
    case Load    extends OpCode(b"001")
    case Store   extends OpCode(b"010")
    case Alu     extends OpCode(b"011")
    case Pop     extends OpCode(b"100")
    case Push    extends OpCode(b"101")
    case Branch  extends OpCode(b"110")
    case Compare extends OpCode(b"111")

  object OpCode:
    def unmask(code: Word) =
      code.unmask(b"1110 0000 0000 0000", 13)

  def isWide(code: Word): Boolean =
    Word.isSet(Wide, code)

  def decode(code: Word): AbstractInstruction = OpCode.unmask(code) match
    case OpCode.Halt.code =>
      HCF
    case OpCode.Load.code if !Word.isSet(11, code) => 
      Load(Source.ImmediateByte(code.lowerByte))
    case OpCode.Load.code if code.unmask(b"1100 1000 0000", 0) == b"1000 0000 0000" =>
      val registerAddress = code.unmask(b"001100000000", 8)
      Load(Source.Register(RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Load.code if code.unmask(b"1100 1000 0000", 0) == b"1000 1000 0000" =>
      val targetAddress = code.unmask(b"0000 0011 0000 0000", 8)
      val sourceAddress = code.unmask(b"0000 0000 0110 0000", 5)
      Move(RegisterName.fromOrdinal(targetAddress), RegisterName.fromOrdinal(sourceAddress))
    case OpCode.Load.code =>
      val registerAddress = code.unmask(b"001100000000", 8)
      val pageAddress     = code.unmask(b"11111111", 0)
      Load(Source.RegisterIndirect(Word.mask(pageAddress), RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Store.code if !Word.isSet(10, code) =>
      val registerAddress = code.unmask(b"001100000000", 8)
      Store(Target.Register(RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Store.code =>
      val registerAddress = code.unmask(b"001100000000", 8)
      val pageAddress     = code.unmask(b"11111111", 0)
      Store(Target.RegisterIndirect(Word.mask(pageAddress), RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Alu.code if Word.isSet(7, code) =>
      val operation = AluOperator.fromOrdinal(code.unmask(b"111100000000", 8))
      val registerAddress = code.unmask(b"11 0000", 4)
      val register = RegisterName.fromOrdinal(registerAddress)
      Arithmetic(operation, AluOperand.Register(register))
    case OpCode.Alu.code => /* SH{R|L} Immediate*/
      val operator = AluOperator.fromOrdinal(code.unmask(b"111100000000", 8))
      val data     = Word.mask(code.unmask(b"1111", 0))
      val operand  = AluOperand.ImmediateWord(data)
      Arithmetic(operator, operand)
    case OpCode.Pop.code if Word.isSet(10, code) =>
      val registerAddress = code.unmask(b"001100000000", 8)
      Pop(PopOperand.Register(RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Pop.code =>
      Pop(PopOperand.Accumulator)
    case OpCode.Push.code if code.unmask(b"110000000000", 10) == b"00" =>
      Push(PushOperand.Accumulator)
    case OpCode.Push.code if code.unmask(b"110000000000", 10) == b"01" =>
      val registerAddress = code.unmask(b"001100000000", 8)
      Push(PushOperand.Register(RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Push.code if code.unmask(b"110000000000", 10) == b"10" =>
      val data = code.lowerByte
      Push(PushOperand.ImmediateByte(data))
    case OpCode.Compare.code if code.unmask(b"1111 0000 0000", 8) == b"00" =>
      Compare(Source.ImmediateByte(code.lowerByte))
    case OpCode.Compare.code if code.unmask(b"1111 0000 0000", 10) == b"10" =>
      val registerAddress = code.unmask(b"001100000000", 8)
      Compare(Source.Register(RegisterName.fromOrdinal(registerAddress)))
    case OpCode.Compare.code =>
      val registerAddress = code.unmask(b"001100000000", 8)
      val offsetRegister  = RegisterName.fromOrdinal(registerAddress)
      val page            = code.lowerByte
      Compare(Source.RegisterIndirect(page, offsetRegister))
    case OpCode.Branch.code =>
      Return
//    case otherwise =>
//      HCF

  /* What is the memory ordering of code0 and code1? */
  def decodeWide(code0: Word, code1: Word): AbstractInstruction =
    OpCode.unmask(code0) match
      case OpCode.Load.code if Word.isSet(10, code0) =>
        val address = Address.computeEffective(code0.lowerByte, code1)
        Load(Source.Memory(address))
      case OpCode.Load.code =>
        Load(Source.ImmediateWord(code1))
      case OpCode.Store.code =>
        val address = Address.computeEffective(code0.lowerByte, code1)
        Store(Target.Memory(address))
      case OpCode.Alu.code =>
        val operator = AluOperator.fromOrdinal(code0.unmask(b"111100000000", 8))
        val operand = AluOperand.ImmediateWord(code1)
        Arithmetic(operator, operand)
      case OpCode.Push.code =>
        Push(PushOperand.ImmediateWord(code1))
      case OpCode.Branch.code if code0.unmask(b"111100000000", 8) == 0 =>
        val address = Address.computeEffective(code0.lowerByte, code1)
        Jump(address)
      case OpCode.Branch.code if code0.unmask(b"111100000000", 8) == b"1000" =>
        val address = Address.computeEffective(code0.lowerByte, code1)
        Call(address)
      case OpCode.Branch.code =>
        val address = Address.computeEffective(code0.lowerByte, code1)
        val flagMask = code0.unmask(b"11100000000", 8)
        val flag     = Flag.fromCode(Word.mask(flagMask))
        Branch(
          address, 
          if Word.isSet(11, code0)
          then Left(flag)
          else Right(flag)
        )
      case OpCode.Compare.code if Word.isSet(10, code0) =>
        val address  = Address.computeEffective(code0.lowerByte, code1)
        Compare(Source.Memory(address))
      case OpCode.Compare.code =>
        Compare(Source.ImmediateWord(code1))