package cp420

opaque type Bus      = Int  /* 24 bit. */
opaque type Word     = Int
opaque type Address  = Int  /* 24 bit. */
opaque type Memory   = IArray[Word]

extension (sc: StringContext)
  /* Only until FromDigits get included. */
  def b(args: Any*): Word =
    val image = sc.s(args*).replace(" ", "")
    Integer.parseInt(image, 2)

object Word:
  extension (data: Word)
    def <  (other: Word): Boolean  = data < other
    def >  (other: Word): Boolean  = data > other
    def +  (other: Word): Int      = data + other
    def -  (other: Word): Int      = data - other
    def *  (other: Word): Int      = data * other
    def /  (other: Word): Int      = data / other
    def %  (other: Word): Int      = data % other
    def &  (other: Word): Int      = data & other
    def |  (other: Word): Int      = data | other
    def ^  (other: Word): Int      = data ^ other
    def << (other: Word): Int      = data << other
    def >> (other: Word): Int      = data >>> other
    def unary_~ : Int = ~data
    def asInt: Int = data
    def lowerByte: Word = (data & 0x00FF)
    def unmask(mask: Word, shift: Int): Int =
      (data & mask.asInt) >>> shift
    def toString = f"${data.toInt}%04X"
  
  def upper(x: Int): Word = 
    (x >>> 16 & 0xFFFF)

  def mask(x: Int): Word =
    (x & 0xFFFF)

  inline def bit(inline n: Int): Word =
    mask(1 << n)

  def isSet(n: Int, in: Word): Boolean =
    (bit(n) & in) != mask(0)


object Address:
  extension (a: Address)
    def next: Address = a + 1
    def previous: Address = a - 1
    def toString = f"$a%06X"
    def offset: Word = Word.mask(a & 0xFFFF)
    def page: Word = Word.mask((a >>> 16) & 0xFF)

  def mask(image: Int): Address =
    image & 0x00FF_FFFF

  def fromWord(w: Word): Address = w

  def computeEffective(page: Word, offset: Word): Address =
    page << 16 | offset

object Bus:
  extension (bus: Bus)
    def data: Word       = Word.mask(bus)
    def address: Address = Address.mask(bus)

  def fromData(x: Word): Bus = x
  def from(x: Address): Bus = x

object Memory:
  extension (memory: Memory)
    def read(address: Address): Word =
      memory(address)
    def write(address: Address, data: Word): Memory =
      memory.updated(address, data)

  def fromWords(data: Word*): Memory = IArray(data*)

object RegisterFile:
  import RegisterName.*

  val initialAccumulator: Word = Word.mask(0x0F0F)

  val initial: Memory =
    val data = Map(
      P -> Word.mask(0x0007),
      Q -> Word.mask(0x0070), 
      R -> Word.mask(0x0700), 
      S -> Word.mask(0x7000),
    )
    Memory.fromWords(data.values.toArray*)

enum Flag:
  case Underflow, Overflow, Zero, Greater, Less, Equal, Halted

  val code = ordinal + 1
  val bit: Word = Word.mask(1 << ordinal)

object Flag:
  def fromCode(word: Word): Flag =
    fromOrdinal(word.asInt - 1)

opaque type Flags = Word

object Flags:
  extension (inline flags: Flags)
    inline def isSet(inline flag: Flag): Boolean =
      isFlagSet(flags, flag)
    inline def set(inline flag: Flag): Flags =
      setFlag(flags, flag)

  def isFlagSet(flags: Flags, flag: Flag) =
    (flags & flag.bit) >>> flag.ordinal == 1

  def setFlag(flags: Flags, flag: Flag) =
    Word.mask(flags | flag.bit)

  val initial: Flags = 0
