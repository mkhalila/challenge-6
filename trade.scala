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
/*val prices = List(28.0, 18.0, 20.0, 26.0, 24.0)
assert(trade_times(prices) == (1, 3), "the trade_times test fails")*/


// (2) Complete the ``get webpage'' function that takes a
// a stock symbol as argument and queries the Yahoo server
// at
//      http://ichart.yahoo.com/table.csv?s=<<insert stock symbol>>
// 
// This servive returns a CSV-list that needs to be separated into
// a list of strings.

/*def get_page(symbol: String): List[String] = ...*/

// (3) Complete the function that processes the CSV list
// extracting the dates and anjusted close prices. The
// prices need to be transformed into Doubles.

/*def process_page(symbol: String): List[(String, Double)] = ...*/


// (4) Complete the query_company function that obtains the
// processed CSV-list for a stock symbol. It should return
// the dates for when to buy and sell the stocks of that company.

/*def query_company(symbol: String): (String, String) =*/


// some test cases

//query_comp("GOOG")

// some more test cases
/*
val indices = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")

for (name <- indices) {
  val times = query_comp(name)
  println(s"Buy ${name} on ${times._1} and sell on ${times._2}")
}
*/


