// Part 2 about Buy-Low-Sell-High using Yahoo Financial Data
//===========================================================


// (1) Complete the function that is given a list of floats
// and calculuates the indices for when to buy the commodity 
// and when to sell

def trade_times(xs: List[Double]): (Int, Int) = {
	val min = xs.indexOf(xs.min)
	val max = xs.indexOf( (xs.drop(min+1)).max )
	(min, max)
}
// an example
val prices = List(28.0, 18.0, 20.0, 26.0, 24.0)
assert(trade_times(prices) == (1, 3), "the trade_times test fails")


// (2) Complete the ``get webpage'' function that takes a
// a stock symbol as argument and queries the Yahoo server
// at
//      http://ichart.yahoo.com/table.csv?s=<<insert stock symbol>>
// 
// This servive returns a CSV-list that needs to be separated into
// a list of strings.
import io.Source
import scala.util.matching.Regex
def get_page(symbol: String): List[String] = {
	val url = "http://ichart.yahoo.com/table.csv?s=" + symbol
  	Source.fromURL(url).mkString.split("\n").toList
}

// (3) Complete the function that processes the CSV list
// extracting the dates and anjusted close prices. The
// prices need to be transformed into Doubles.

def process_page(symbol: String): List[(String, Double)] = {
	val list = get_page(symbol).drop(1)
	var list3 = List(("", 1.0))
	for( i <- list ) {
		val list2 = i.split(",").toList
		val x = list3
		list3 = (list2(0), list2(6).toDouble)::x
	}
	list3.reverse.drop(1).reverse
}

// (4) Complete the query_company function that obtains the
// processed CSV-list for a stock symbol. It should return
// the dates for when to buy and sell the stocks of that company.
import scala.util._
def query_company(symbol: String): (String, String) = {
	val datesAndPrices = process_page(symbol)
	var prices = List(1.0)
	for( i <- (0 until datesAndPrices.length)) {
		val x = prices
		prices = datesAndPrices(i)._2 :: x
	}
	prices = prices.reverse.drop(1)
	val buy = trade_times(prices)._1
	val sell = trade_times(prices)._2
	val buyDate = datesAndPrices(buy)._1
	val sellDate = datesAndPrices(sell)._1
	(buyDate, sellDate)
}

// some test cases

query_company("GOOG")

// some more test cases

val indices = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")

for (name <- indices) {
  val times = query_company(name)
  println(s"Buy ${name} on ${times._1} and sell on ${times._2}")
}



