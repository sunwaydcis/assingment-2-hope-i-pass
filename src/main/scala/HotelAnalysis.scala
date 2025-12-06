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
    val filename = "hotel_data.csv"

    // ISO-8859-1 needed for some currency symbols
    val fileAttempt = Try(Source.fromFile(filename)("ISO-8859-1"))

    fileAttempt match {
      case Success(source) =>
        val lines = source.getLines().drop(1)

        // flatmap drops any lines that failed to parse
        val bookings = lines.flatMap(parseLine).toList
        source.close()

        // CHANGE: Now we actually call the function
        if (bookings.nonEmpty) processData(bookings)
        else println("Error: Dataset is empty.")

      case Failure(e) =>
        println(s"File read error: ${e.getMessage}")
    }
  }

  def parseLine(line: String): Option[HotelBooking] = {
    // regex to split commas but ignore ones inside quotes
    val regex = ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)".r
    val cols = regex.split(line).map(_.trim.replace("\"", ""))

    Try {
      // clean up percentage formats
      val discountVal = if (cols(21).contains("%")) {
        cols(21).replace("%", "").toDouble / 100
      } else {
        cols(21).toDouble
      }

      HotelBooking(
        country   = cols(9),
        people    = cols(11).toInt,
        hotelName = cols(16),
        price     = cols(20).toDouble,
        discount  = discountVal,
        margin    = cols(23).toDouble
      )
    }.toOption
  }

  // NEW FUNCTION: Logic for Question 1 only
  def processData(data: List[HotelBooking]): Unit = {
    println("--- Hotel Analysis Results ---\n")

    // Question 1
    val topCountry = data.groupBy(_.country).maxBy(_._2.size)
    println(s"1. Highest Bookings: ${topCountry._1} (${topCountry._2.size})")
  }
}