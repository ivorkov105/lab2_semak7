package CustomTypes

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

case class CustomDate private (date: LocalDate) extends Ordered[CustomDate] {
  
  def getDay: Int = date.getDayOfMonth
  
  def getMonth: Int = date.getMonthValue
  
  def getYear: Int = date.getYear
  
  def addDays(days: Int): CustomDate = CustomDate(date.plusDays(days))
  
  def subtractDays(days: Int): CustomDate = CustomDate(date.minusDays(days))
  
  override def compare(that: CustomDate): Int = this.date.compareTo(that.date)
  
  override def toString: String = date.format(CustomDate.FORMATTER)
}

object CustomDate {
  private val FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  
  def of(day: Int, month: Int, year: Int): CustomDate = {
    CustomDate(LocalDate.of(year, month, day))
  }
  
  def parse(dateString: String): CustomDate = {
    CustomDate(LocalDate.parse(dateString, FORMATTER))
  }
}
