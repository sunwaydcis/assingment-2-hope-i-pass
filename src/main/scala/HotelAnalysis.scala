import scala.io.Source
import scala.util.{Try, Success, Failure}

case class HotelBooking(
                         country: String,
                         people: Int,
                         hotelName: String,
                         price: Double,
                         discount: Double,
                         margin: Double
                       )

object HotelAnalysis {
  def main(args: Array[String]): Unit = {
    println("Hotel Analysis App Initialized...")
    // TODO: Implement file reading logic
  }
}