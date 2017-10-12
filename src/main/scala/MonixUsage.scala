object RxUsage extends App{

  import rx._

  val maxSpins = 100
  val blackNumbers = List(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)
  val redNumbers = List(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  var spinHitsMap = Map((0 to 37).toList.zip((0 to 37).toList.map{x => 0}).map{case (x,y) => (x -> y)}: _*)

  val numberMap: Map[String,Int] = Map(" 0" -> 0, "00" -> 0, " 1" -> 1, " 2" -> 2, " 3" -> 3, " 4" -> 4,
                                       " 5" -> 5, " 6" -> 6, " 7" -> 7, " 8" -> 8, " 9" -> 9, "01" -> 1,
                                       "02" -> 2, "03" -> 3, "04" -> 4, "05" -> 5, "06" -> 6, "07" -> 7,
                                       "08" -> 8, "09" -> 9, "10" -> 10, "11" -> 11, "12" -> 12, "13" -> 13,
                                       "14" -> 14, "15" -> 15, "16" -> 16, "17" -> 17, "18" -> 18, "19" -> 19,
                                       "20" -> 20, "21" -> 21, "22" -> 22, "23" -> 23, "24" -> 24, "25" -> 25,
                                       "26" -> 26, "27" -> 27, "28" -> 28, "29" -> 29, "30" -> 30, "31" -> 31,
                                       "32" -> 32, "33" -> 33, "34" -> 34, "35" -> 35, "36" -> 36, "37" -> 37
                                      )


  //Level 0
  val spinResults : Var[Seq[String]] = Var(Seq.empty[String])

  //Level 1
  val lastWinNumber = Rx(spinResults().headOption)
  val spinCount = Rx(spinResults().take(maxSpins).length)
  val zeroCount = Rx(spinResults().take(maxSpins).count(x => {numberMap(x) == 0}))
  val evenCount = Rx(spinResults().take(maxSpins).count(x => {(numberMap(x)%2 == 0) & (numberMap(x) != 0)}))
  val oddCount = Rx(spinResults().take(maxSpins).count(x => {numberMap(x)%2 != 0}))
  val redCount = Rx(spinResults().take(maxSpins).count(x => {redNumbers.contains(numberMap(x))}))
  val blackCount = Rx(spinResults().take(maxSpins).count(x => {blackNumbers.contains(numberMap(x))}))
  val oneTo18Count = Rx(spinResults().take(maxSpins).count(x => {1 until 18 contains(numberMap(x))}))
  val eighteenTo36Count = Rx(spinResults().take(maxSpins).count(x => {18 until 36 contains(numberMap(x))}))
  def hot4 = spinHitsMap.toSeq.sortBy(_._2).takeRight(4)
  def cold4 = spinHitsMap.toSeq.sortBy(_._2).take(4)

  spinResults.trigger{
    val number = numberMap(lastWinNumber.now.getOrElse("37"))
    val currentValue = spinHitsMap.get(numberMap(lastWinNumber.now.getOrElse("00")))
    spinHitsMap = spinHitsMap.updated(number, currentValue.get + 1)
    println("-->" + spinHitsMap)
  }

  //Level 2
  val oddPercentage = Rx((100*oddCount()/spinCount()))
  val evenPercentage = Rx((100*evenCount())/spinCount())
  val zeroPercentage = Rx((100*zeroCount())/spinCount())

  val redPercentage = Rx((100*redCount())/spinCount())
  val blackPercentage = Rx((100*blackCount())/spinCount())

  spinResults() = Seq(" 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 3"," 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 3"," 3"," 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)

  spinResults() = Seq(" 3"," 3"," 3"," 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 0"," 3"," 3"," 3"," 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq("00"," 0"," 3"," 3"," 3"," 2"," 1")

  println(lastWinNumber.now.getOrElse("--"),spinCount.now,evenCount.now,zeroCount.now,oddCount.now,redCount.now,blackCount.now)
  println(oddPercentage.now,evenPercentage.now,zeroPercentage.now,redPercentage.now,blackPercentage.now)
  println(spinHitsMap)
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)



}
