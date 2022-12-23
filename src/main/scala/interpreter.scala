package cp420

/* Are there ports? */
/* Especially mapped memory regions for consoles? */
/* This could have a current (decoded) instruction. */
case class MachineState(
  memory:       Memory,
  registerFile: Memory,
  bus:          Bus,

  status:       Flags,

  accumulator:  Word,

  pc:           Address,
  sp:           Address,
) extends AbstractMachine

trait AbstractMachine:
  state: MachineState =>
  import AbstractInstruction.*

  type Machine = AbstractMachine & MachineState

  def setAddress(address: Address) =
    copy(bus = Bus.from(address))

  def fetch: Machine =
    val data = memory.read(bus.address)
    copy(bus = Bus.fromData(data))

  def store(data: Word): Machine =
    copy(memory = memory.write(bus.address, data))

  def step: Machine =
    copy(pc = pc.next)

  def jump =
    copy(pc = bus.address)

  def push: Machine =
    copy(sp = sp.previous)

  def pushWord(data: Word): Machine =
    setAddress(sp).store(data).push

  def pop: Machine =
    copy(sp = sp.next)

  def clearFlags: Machine =
    copy(status = Flags.initial)

  def setFlag(flag: Flag): Machine =
    copy(status = status.set(flag))

  def setAccumulator(data: Word): Machine =
    copy(accumulator = data)

  def setRegister(register: RegisterName, data: Word): Machine =
    copy(registerFile = registerFile.write(register.address, data))

  def getRegister(register: RegisterName): Word =
    registerFile.read(register.address)

  def compareAndSetFlags(data: Word): Machine =
    val m = clearFlags
    if m.accumulator < data then m.setFlag(Flag.Less)
    else if m.accumulator > data then m.setFlag(Flag.Greater)
    else if m.accumulator == Word.mask(0) then 
      m.setFlag(Flag.Equal).setFlag(Flag.Zero)
    else m.setFlag(Flag.Equal)

  def arithmetic(operation: Operator, data: Word): Machine = operation match
    case Operator.Add =>
      val result = accumulator + data
      val m = setAccumulator(Word.mask(result))
      if result > Short.MaxValue then m.setFlag(Flag.Overflow)
      else if result < Short.MinValue then m.setFlag(Flag.Underflow)
      else if result == 0 then m.setFlag(Flag.Zero)
      else this
    case Operator.Subtract =>
      val result = accumulator - data
      val m = setAccumulator(Word.mask(result))
      if result > Short.MaxValue then m.setFlag(Flag.Overflow)
      else if result < Short.MinValue then m.setFlag(Flag.Underflow)
      else if result == 0 then m.setFlag(Flag.Zero)
      else this
    case Operator.Multiply =>
      val result = data * accumulator
      val m = setAccumulator(Word.mask(result))
      // result in: Q:accumulator.
      m.setRegister(RegisterName.Q, Word.upper(result))
    case Operator.Divide  =>
      val result = accumulator / data
      val remainder = accumulator % data
      val m = setAccumulator(Word.mask(result))
      // result in: accumulator; remainder in Q.
      m.setRegister(RegisterName.Q, Word.mask(remainder))
    case Operator.Modulo =>
      setAccumulator(Word.mask(accumulator % data))
    case Operator.And =>
      setAccumulator(Word.mask(accumulator & data))
    case Operator.Or =>
      setAccumulator(Word.mask(accumulator | data))
    case Operator.Xor =>
      setAccumulator(Word.mask(accumulator ^ data))
    case Operator.Not =>
      setAccumulator(Word.mask(~data))
    case Operator.Shl =>
      setAccumulator(Word.mask(accumulator << data))
    case Operator.Shr =>
      setAccumulator(Word.mask(accumulator >> data))

  def interpret(instruction: AbstractInstruction): Machine = instruction match
    case HCF =>
      setFlag(Flag.Halted)
    case Load(Source.ImmediateByte(data)) =>
      setAccumulator(data)
    case Load(Source.ImmediateWord(data)) =>
      setAccumulator(data)
    case Load(Source.Memory(address)) =>
      val m = setAddress(address).fetch
      m.setAccumulator(m.bus.data)
    case Load(Source.Register(register)) =>
      setAccumulator(getRegister(register))
    case Load(Source.RegisterIndirect(pageAddress, offsetRegister)) =>
      val offset  = getRegister(offsetRegister)
      val address = Address.computeEffective(pageAddress, offset)
      val m = setAddress(address).fetch
      m.setAccumulator(m.bus.data)
    case Store(Target.Memory(address)) =>
      setAddress(address).store(accumulator)
    case Store(Target.Register(register)) =>
      setRegister(register, accumulator)
    case Store(Target.RegisterIndirect(pageAddress, offsetRegister)) =>
      val offset  = getRegister(offsetRegister)
      val address = Address.computeEffective(pageAddress, offset)
      setAddress(address).store(accumulator)
    case Pop(PopOperand.Accumulator) =>
      val m0 = pop
      val m = m0.setAddress(m0.sp).fetch
      m.setAccumulator(m.bus.data)
    case Pop(PopOperand.Register(register)) =>
      val m = setAddress(sp).fetch.pop
      m.setRegister(register, m.bus.data)
    case Push(PushOperand.Accumulator) =>
      setAddress(sp).store(accumulator).push
    case Push(PushOperand.Register(register)) =>
      setAddress(sp).store(getRegister(register)).push        
    case Push(PushOperand.ImmediateByte(data)) =>
      setAddress(sp).store(data).push
    case Push(PushOperand.ImmediateWord(data)) =>
      setAddress(sp).store(data).push
    case Jump(address) =>
      setAddress(address).jump
    case Call(address) =>
      pushWord(pc.offset)
        .pushWord(pc.page)
        .setAddress(address)
        .jump
    case Return =>
      val m = setAddress(sp).fetch.pop
      val page = m.bus.data.lowerByte
      val m1 = m.setAddress(m.sp).fetch.pop
      val offset = m1.bus.data
      val returnAddress = Address.computeEffective(page, offset)
      m1.setAddress(returnAddress).jump
    case Compare(Source.ImmediateByte(data)) =>
      compareAndSetFlags(data)
    case Compare(Source.ImmediateWord(data)) =>
      compareAndSetFlags(data)
    case Compare(Source.Memory(address)) =>
      val m = setAddress(address).fetch
      m.compareAndSetFlags(m.bus.data)
    case Compare(Source.Register(register)) =>
      compareAndSetFlags(getRegister(register))
    case Compare(Source.RegisterIndirect(page, offsetRegister)) =>
      val offset  = getRegister(offsetRegister)
      val address = Address.computeEffective(page, offset)
      val m       = setAddress(address).fetch
      m.compareAndSetFlags(m.bus.data)
    case Branch(target, Left(flag)) =>
      if !status.isSet(flag)
        then setAddress(target).jump
        else this
    case Branch(target, Right(flag)) =>
      if status.isSet(flag)
        then setAddress(target).jump
        else this
    case Arithmetic(operation, AluOperand.ImmediateWord(data)) =>
      clearFlags.arithmetic(operation, data)
    case Arithmetic(operation, AluOperand.Register(register)) =>
      clearFlags.arithmetic(operation, getRegister(register))
//    case otherwise => ???

  /* This begs for a State monad. */
  def cycle: Option[Machine] =
    val m = setAddress(pc).fetch.step
    val code0 = m.bus.data
    val m2 = if InstructionDecoder.isWide(code0) then
      val m1 = m.setAddress(pc).fetch.step
      val code1 = m1.bus.data
      m1.interpret(InstructionDecoder.decodeWide(code0, code1))
    else
      m.interpret(InstructionDecoder.decode(code0))

    Option(m2)

object AbstractMachine:
  def bootstrap(
    startAddress: Address,
    stack:        Address,
    memory:       Memory
  ): AbstractMachine = MachineState(
    accumulator  = RegisterFile.initialAccumulator,
    registerFile = RegisterFile.initial,
    status       = Flags.initial,
    pc           = startAddress,
    sp           = stack,
    bus          = Bus.from(startAddress),
    memory       = memory,
  )