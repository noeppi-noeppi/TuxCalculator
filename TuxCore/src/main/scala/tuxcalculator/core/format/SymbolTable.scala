package tuxcalculator.core.format

import java.io._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SymbolTable[T] private(private[this] val isMutable: Boolean, val name: String, private[this] val encoder: SymbolEncoder[T]) {

  private[this] val items: ListBuffer[T] = ListBuffer()
  private[this] val map: mutable.Map[T, Int] = mutable.Map()
  
  private[this] var dataIn: DataInputStream = _
  private[this] var maxLen: Int = 0
  private[this] val dataOut: ByteArrayOutputStream = new ByteArrayOutputStream()
  
  private[this] var readingAhead: Boolean = false
  
  def this(name: String, encoder: SymbolEncoder[T], in: Option[DataInput]) = {
    this(isMutable = in.isEmpty, name, encoder)
    in match {
      case Some(input) =>
        val byteLen = input.readInt()
        maxLen = input.readInt()
        val allData = new Array[Byte](byteLen)
        input.readFully(allData)
        dataOut.write(allData)
        dataIn = new DataInputStream(new ByteArrayInputStream(allData))
      case None =>
    }
  }

  def add(value: T): Int = map.get(value) match {
    case Some(id) => id
    case None =>
      if (!isMutable) throw new IllegalStateException("Can't extend immutable symbol table")
      // Write to extra stream, as the write method may add new
      // items to the table
      val theData = new ByteArrayOutputStream()
      val theOut = new DataOutputStream(theData)
      encoder.write(value, theOut)
      theOut.close()
      dataOut.write(theData.toByteArray)
      items.addOne(value)
      map(value) = items.length - 1
      maxLen = items.length
      items.length - 1
  }
  
  def get(id: Int): T = {
    if (items.indices.contains(id)) {
      items(id)
    } else if (id >= 0 && id < maxLen) {
      if (readingAhead) {
        throw new InvalidFormatException("Invalid symbol table order: Forward reference in " + name)
      } else {
        // Read ahead until id
        readingAhead = true
        for (i <- items.length to id) {
          val value = encoder.read(dataIn)
          items.addOne(value)
          map(value) = i
        }
        readingAhead = false
        items(id)
      }
    } else {
      throw new InvalidFormatException("Invalid " + name + " symbol table entry: " + id + " (max is " + maxLen + ")")
    }
  }
  
  def write(out: DataOutput): Unit = {
    val theData = dataOut.toByteArray
    out.writeInt(theData.length)
    out.writeInt(items.length)
    out.write(theData)
  }
}

trait SymbolEncoder[T] {

  def read(in: DataInput): T
  def write(value: T, out: DataOutput): Unit
}
