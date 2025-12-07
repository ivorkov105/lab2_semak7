package CustomTypes

import com.google.gson._
import java.lang.reflect.Type

object Point2DAdapter extends JsonSerializer[Point2D] with JsonDeserializer[Point2D] {
  
  override def serialize(src: Point2D, typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
    new JsonPrimitive(s"${src.getX},${src.getY}")
  }
  
  override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Point2D = {
    Point2D.parse(json.getAsString)
  }
}
