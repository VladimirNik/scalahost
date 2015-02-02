package scala.meta.internal.hosts.scalac.convert

import java.io.{File, FileOutputStream, BufferedOutputStream, BufferedWriter}
import scala.tools.nsc.Global

trait Deserialize extends SerializerUtils{

  val global: Global

  import global._

  import org.tukaani.xz.{LZMA2Options, XZOutputStream}
  import scala.reflect.internal.pickling.PickleBuffer

  import scala.collection.immutable.SortedMap
  import scala.collection.mutable
  import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
  import scala.runtime.BoxedUnit

  val deserialiazer = new BinaryDeserializer

  def deserialize(file: File) = deserialiazer.deserialize(file)

  class BinaryDeserializer {

    val trees = new PickleBuffer(new Array[Byte](1024), 0, 0)

    def convertToString(ar: Array[Byte]): String = new String(ar, "UTF-8")

    def deserialize(file: File): Unit = {
      import java.nio.file.Files
      import java.nio.file.Paths
      import java.nio.file.Path

      val path = Paths.get(file.getPath)
      val data = Files.readAllBytes(path)

      val input = new PickleBuffer(data, 0, 0)

      val magic: Int = input.readRawInt
      val version: Int = input.readRawInt

      println(s"Magic: $magic, should be: ${0x000A11CE}")
      println(s"version: $version, should be: 0")

      def readAE(pb: PickleBuffer): Array[Byte] = {
        val size = pb.readRawInt
        pb.readBytes(size)
      }
      def readByteArray(pb: PickleBuffer): Array[Byte] = readAE(pb)
      def readString(pb: PickleBuffer) = convertToString(readByteArray(pb))
      def readPB(pb: PickleBuffer): PickleBuffer = new PickleBuffer(readAE(pb), 0, 0)

      val compilerNameBytes = readString(input)

      val constantsSectionNameBytes = readString(input)
      val serializedConstants = readPB(input)

      //TODO - methods to deserialize strings section
      def readConstant: Constant = {
        val constType = serializedConstants.readByte()
        constType match {
          //Unit
          case 0 => Constant(())
          //Null
          case 1 => Constant(null)
          //True
          case 2 => Constant(true)
          //False
          case 3 => Constant(false)
          //Byte
          case 4 => Constant(serializedConstants.readByte())
          //Short
          case 5 =>
            //TODO - check
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = (((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)).toShort
            Constant(value)
          //Char
          case 6 =>
            val value = readString(serializedConstants)
            //TODO - maybe rewrite
            Constant(value.toCharArray().apply(0))
          //Int
          case 7 =>
            //TODO - check
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            Constant(value)
          //Long
          case 8 =>
            //TODO - check
            val i56 = serializedConstants.readByte()
            val i48 = serializedConstants.readByte()
            val i40 = serializedConstants.readByte()
            val i32 = serializedConstants.readByte()
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val value = ((i56 & 0xFF) << 56).toLong + ((i48 & 0xFF) << 48).toLong + ((i40 & 0xFF) << 40).toLong + ((i32 & 0xFF) << 32).toLong +
              ((i24 & 0xFF) << 24).toLong + ((i16 & 0xFF) << 16).toLong + ((i8 & 0xFF) << 8).toLong + ((i0 & 0xFF) << 0).toLong
            Constant(value)
          //Float
          case 9 =>
            //TODO - rewrite
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val intBits = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val res = java.lang.Float.intBitsToFloat(intBits)
            Constant(res)
          //Double
          case 10 =>
            //TODO - rewrite
            val i56 = serializedConstants.readByte()
            val i48 = serializedConstants.readByte()
            val i40 = serializedConstants.readByte()
            val i32 = serializedConstants.readByte()
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val longBits = ((i56 & 0xFF) << 56) + ((i48 & 0xFF) << 48) + ((i40 & 0xFF) << 40) + ((i32 & 0xFF) << 32) +
              ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val res = java.lang.Double.longBitsToDouble(longBits)
            Constant(res)
          //String
          case 11 =>
            //TODO - check
            val i24 = serializedConstants.readByte()
            val i16 = serializedConstants.readByte()
            val i8 = serializedConstants.readByte()
            val i0 = serializedConstants.readByte()
            val stringId = ((i24 & 0xFF) << 24) + ((i16 & 0xFF) << 16) + ((i8 & 0xFF) << 8) + ((i0 & 0xFF) << 0)
            val value = getStringById(stringId)
            //TODO - maybe rewrite
            Constant(value)
        }
      }

      val stringsSectionNameBytes = readString(input)
      val serializedStrings = readPB(input)

      def deserializeConstants: mutable.Map[Int, Constant] = {
        val list = serializedConstants.until(serializedConstants.bytes.size,
          () => {
            val ind = serializedConstants.readIndex
            val const = readConstant
            (ind, const)
          }
        )
        mutable.Map(list: _*)
      }

      //TODO - method to deserialize strings section
      def deserializeStrings: mutable.Map[Int, String] = {
        val list = serializedStrings.until(serializedStrings.bytes.size,
          () => {
            val ind = serializedStrings.readIndex
            val str = readString(serializedStrings)
            (ind, str)
          }
        )
        mutable.Map(list: _*)
      }

      //strings should be initialized before constants
      strings ++= (deserializeStrings)
      constants ++= (deserializeConstants)

      println(s"constants: $constants")
      println(s"strings: $strings")

      //TODO: trees processing
    }

    def init(unit: Tree): Unit = {
    }

    val serializedSymbols = new PickleBuffer(new Array[Byte](1024), 0, 0)

    def getSymbolById(id: Int): Symbol = {
      ???
    }

//    def deserializeSymbol(nd: ArrayBuffer[Byte]): Symbol = {
//      val id = (nd: @unchecked) match {
//        case NoSymbol => 0
//        case _ if nd.isRoot => 1 //TODO-check isRoot for NoSymbol and RootClass equality (should be false, true)
//        //Local
//        //TODO - rewrite !sym.isLocatable
//        case sym: Symbol if !sym.isLocatable => 2
//        //External Type
//        case sym: Symbol if sym.isType => 3
//        //External Term
//        case sym: Symbol if sym.isTerm => 4
//        case sym: Symbol if sym.hasPackageFlag => 5
//        //case array => 6 //TODO implement for Array
//      }
//      serializedSymbols.writeByte(id)
//    }

    //TODO implement modifiers rewriting
    def readModifiersId(mod: Modifiers, idx: Int): Unit = ???

    //get during deserialization
    val strings: mutable.Map[Int, String] = new mutable.HashMap[Int, String]()

    //TODO - method to get string from strings Map[Offset, String]
    def getStringById(id: Int): String = strings(id)

    //get during deserialization
    val constants: mutable.Map[Int, Constant] = new mutable.HashMap[Int, Constant]()

    //TODO - method to get constant from constants Map[Offset, Constant]
    def getCostantById(id: Int): Constant = constants(id)

//    def deserializeTrees(c: Array[Byte]): Constant = {
//      val id = (nd: @unchecked) match {
//        case EmptyTree => 0
//        case _: Ident@unchecked => 1
//        case _: Select@unchecked | _: TypeRef => 2
//        case _: This@unchecked => 3
//        case _: Super@unchecked => 4
//        case _: Apply@unchecked => 5
//        case _: UnApply@unchecked => 6
//        //          case _: NamedArg@unchecked => 7
//        //          case _: SeqLiteral@unchecked => 8
//        case _: TypeApply@unchecked => 9
//        case _: Literal@unchecked => 10
//        case _: New@unchecked => 11
//        case _: Typed@unchecked => 12
//        case _: Assign@unchecked => 13
//        case _: Block@unchecked => 14
//        case _: If@unchecked => 15
//        //          case _: Closure@unchecked => 16
//        case _: Match@unchecked => 17
//        case _: CaseDef@unchecked => 18
//        case _: Return@unchecked => 19
//        case _: Try@unchecked => 20
//        case _: Throw@unchecked => 21
//        case _: Bind@unchecked => 22
//        case _: Alternative@unchecked => 23
//        //24 is annotation
//        case _: ValDef@unchecked => 25
//        case _: DefDef@unchecked => 26
//        case _: TypeDef@unchecked => 27
//        case _: ModuleDef@unchecked => 28
//        case _: Template@unchecked => 29
//        case _: PackageDef@unchecked => 30
//        case _: Import@unchecked => 31
//        //          case _: Pair@unchecked => 32
//        case _: AnnotatedType@unchecked => 33
//        case _: SingletonType@unchecked => 34
//        case _: SelectFromTypeTree@unchecked => 35
//        //          case _: AndType@unchecked => 36
//        //          case _: OrType@unchecked => 37
//        case _: RefinedType@unchecked => 38
//        //          case _: ByNameTypeTree@unchecked => 39
//        //case _: RepeatedType => 40
//        case _: TypeBounds@unchecked => 41
//        //case _: ExistentialType => 42
//        //case _: AppliedType => 43
//      }
//      trees.writeByte(id)
//    }

    //from trees
    def readBoolean: Int = {
      ???
    }

    //from trees
    def readArgsCount: Int = {
      ???
    }
  }

}
