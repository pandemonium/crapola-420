package cp420

enum RegisterName:
  case P, Q, R, S

  val address: Address = Address.mask(ordinal.toShort)

enum Source:
  case ImmediateByte(byte: Byte)  /* Not needed. */
  case ImmediateWord(word: Word)
  case Memory(address: Address)
  case Register(register: RegisterName)
  case RegisterIndirect(page: Byte, offsetRegister: RegisterName)

enum Target:
  case Memory(address: Address)
  case Register(register: RegisterName)
  case RegisterIndirect(page: Byte, offsetRegister: RegisterName)

enum PopOperand:
  case Accumulator
  case Register(register: RegisterName)

enum PushOperand:
  case Accumulator
  case Register(register: RegisterName)
  case ImmediateByte(byte: Byte)
  case ImmediateWord(word: Word)

enum AluOperand:
  case ImmediateWord(w: Word)
  case Register(register: RegisterName)

enum Operator(val opCode: Word):
  case Add      extends Operator(b"0000")
  case Subtract extends Operator(b"0001")
  case Multiply extends Operator(b"0010")
  case Divide   extends Operator(b"0011")
  case Modulo   extends Operator(b"0100")
  case And      extends Operator(b"0101")
  case Or       extends Operator(b"0110")
  case Xor      extends Operator(b"0111")
  case Not      extends Operator(b"1000")
  case Shl      extends Operator(b"1001")
  case Shr      extends Operator(b"1010")

enum AbstractInstruction:
  case /* 0x00 */ HCF
  case /* 0x01 */ Load(source: Source)
  case /* 0x02 */ Store(target: Target)
  case /* 0x03 */ Arithmetic(operation: Operator, operand: AluOperand)
  case /* 0x04 */ Pop(target: PopOperand)
  case /* 0x05 */ Push(source: PushOperand)
  case /* 0x06 */ Jump(target: Address)
  case /* 0x06 */ Call(target: Address)
  case /* 0x06 */ Return
  case /* 0x06 */ Branch(target: Address, flag: Either[Flag, Flag])
  case /* 0x07 */ Compare(source: Source)
