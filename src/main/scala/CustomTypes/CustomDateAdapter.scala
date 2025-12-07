package CustomTypes

import com.google.gson._
import java.lang.reflect.Type

object CustomDateAdapter extends JsonSerializer[CustomDate] with JsonDeserializer[CustomDate] {
  
  override def serialize(src: CustomDate, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    new JsonPrimitive(src.toString) // Формат dd/MM/yyyy
  }
  
  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): CustomDate = {
    CustomDate.parse(json.getAsString)
  }
}
