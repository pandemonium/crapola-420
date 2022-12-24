package cp420

enum RegisterName:
  case P, Q, R, S

  val address: Address = Address.mask(ordinal)

enum Source:
  case ImmediateByte(byte: Word)  /* Not needed. */
  case ImmediateWord(word: Word)
  case Memory(address: Address)
  case Register(register: RegisterName)
  case RegisterIndirect(page: Word, offsetRegister: RegisterName)

enum Target:
  case Memory(address: Address)
  case Register(register: RegisterName)
  case RegisterIndirect(page: Word, offsetRegister: RegisterName)

enum PopOperand:
  case Accumulator
  case Register(register: RegisterName)

enum PushOperand:
  case Accumulator
  case Register(register: RegisterName)
  case ImmediateByte(byte: Word)
  case ImmediateWord(word: Word)

enum AluOperand:
  case ImmediateWord(w: Word)
  case Register(register: RegisterName)

enum AluOperator(val opCode: Word):
  case Add      extends AluOperator(b"0000")
  case Subtract extends AluOperator(b"0001")
  case Multiply extends AluOperator(b"0010")
  case Divide   extends AluOperator(b"0011")
  case Modulo   extends AluOperator(b"0100")
  case And      extends AluOperator(b"0101")
  case Or       extends AluOperator(b"0110")
  case Xor      extends AluOperator(b"0111")
  case Not      extends AluOperator(b"1000")
  case Shl      extends AluOperator(b"1001")
  case Shr      extends AluOperator(b"1010")

enum AbstractInstruction:
  case /* 0x00 */ HCF
  case /* 0x01 */ Load(source: Source)
  case /* 0x01 */ Move(target: RegisterName, source: RegisterName)
  case /* 0x02 */ Store(target: Target)
  case /* 0x03 */ Arithmetic(operation: AluOperator, operand: AluOperand)
  case /* 0x04 */ Pop(target: PopOperand)
  case /* 0x05 */ Push(source: PushOperand)
  case /* 0x06 */ Jump(target: Address)
  case /* 0x06 */ Call(target: Address)
  case /* 0x06 */ Return
  case /* 0x06 */ Branch(target: Address, flag: Either[Flag, Flag])
  case /* 0x07 */ Compare(source: Source)
