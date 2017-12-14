package gson

import com.google.gson.stream.{JsonReader, JsonWriter}
import com.google.gson.{GsonBuilder, TypeAdapter}
import gson.GsonEnumAdapter.DealType.DealType

object GsonEnumAdapter extends App {

  object DealType extends Enumeration {
    type DealType = Value
    val WORLD_BETTER, WORLD_CLEAN = Value
  }

  class EnumAdapter[T <: scala.Enumeration](values: T#ValueSet) extends TypeAdapter[T#Value] {
    override def read(in: JsonReader): T#Value = {
      values.view.filter((_: T#Value).toString == in.nextString).head
    }

    override def write(out: JsonWriter, value: T#Value) = {
      out.value(value.toString)
    }
  }

  val gson = new GsonBuilder()
                    .serializeNulls()
                    .registerTypeHierarchyAdapter(classOf[DealType], new EnumAdapter(DealType.values))
                    .create()

  val str = gson.toJson(DealType.WORLD_BETTER)
  println(s"str = $str")

  val obj = gson.fromJson(str, classOf[DealType])
  println(s"obj = $obj")
}