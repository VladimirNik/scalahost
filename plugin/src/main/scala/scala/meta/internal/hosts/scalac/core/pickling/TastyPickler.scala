package scala.meta.internal.hosts.scalac
package core
package pickling

import PickleFormat._
import collection.mutable
import TastyBuffer._

class TastyPickler {
  self: NameBuffers with TastyNames =>

  private val sections = new mutable.ArrayBuffer[(TastyName.NameRef, TastyBuffer)]
  
  private val headerBuffer = {
    val buf = new TastyBuffer(16)
    for (ch <- header) buf.writeByte(ch.toByte)
    buf.writeNat(MajorVersion)
    buf.writeNat(MinorVersion)
    buf
  }

  val nameBuffer = new NameBuffer
  
  def newSection(name: String, buf: TastyBuffer) = 
    sections += ((nameBuffer.nameIndex(name), buf))
  
  def assembleParts(): Array[Byte] = {
    def lengthWithLength(buf: TastyBuffer) = {
      buf.assemble()
      buf.length + natSize(buf.length)
    }
    val totalSize = 
      headerBuffer.length + 
      lengthWithLength(nameBuffer) + {
        for ((nameRef, buf) <- sections) yield
          natSize(nameRef.index) + lengthWithLength(buf)
      }.sum
    val all = new TastyBuffer(totalSize)
    all.writeBytes(headerBuffer.bytes, headerBuffer.length)
    all.writeNat(nameBuffer.length)
    all.writeBytes(nameBuffer.bytes, nameBuffer.length)
    for ((nameRef, buf) <- sections) {
      all.writeNat(nameRef.index)
      all.writeNat(buf.length)
      all.writeBytes(buf.bytes, buf.length)
    }
    assert(all.length == totalSize && all.bytes.length == totalSize, s"totalSize = $totalSize, all.length = ${all.length}, all.bytes.length = ${all.bytes.length}")
    all.bytes
  }
}