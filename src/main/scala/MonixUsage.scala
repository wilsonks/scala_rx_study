


import rx._

import scala.collection.mutable.{Map => MMap}


object RxUsage extends App{



  val maxSpins = 100

  val singleZeroWheel : List[String] = List(" 0","32","15","19"," 4","21"," 2","25","17","34"," 6","27","13","36","11","30"," 8", "23",
                                          "10"," 5","24","16","33"," 1","20","14","31"," 9","22","18","29"," 7","28","12","35"," 3","26")
  val doubleZeroWheel : List[String] = List(" 0","28"," 9","26","30","11"," 7","20","32","17"," 5","22","34","15"," 3","24","36","13"," 1","00",
                                          "27","10","25","29","12"," 8","19","31","18"," 6","21","33","16"," 4","23","35","14"," 2")


  val emptyspinHitsMap = Map(doubleZeroWheel.zip((0 to 38).toList.map(x => 0)).map{case (x,y) => x -> y}: _*)

  println(emptyspinHitsMap)

  val blackNumbers = List(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)
  val redNumbers = List(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)

  var spinHitsMap = Map((0 to 37).toList.zip((0 to 37).toList.map{x => 0}).map{case (x,y) => x -> y}: _*)

  val numberMap: Map[String,Int] = Map(" 0" -> 0, "00" -> 0, " 1" -> 1, " 2" -> 2, " 3" -> 3, " 4" -> 4,
                                       " 5" -> 5, " 6" -> 6, " 7" -> 7, " 8" -> 8, " 9" -> 9, "01" -> 1,
                                       "02" -> 2, "03" -> 3, "04" -> 4, "05" -> 5, "06" -> 6, "07" -> 7,
                                       "08" -> 8, "09" -> 9, "10" -> 10, "11" -> 11, "12" -> 12, "13" -> 13,
                                       "14" -> 14, "15" -> 15, "16" -> 16, "17" -> 17, "18" -> 18, "19" -> 19,
                                       "20" -> 20, "21" -> 21, "22" -> 22, "23" -> 23, "24" -> 24, "25" -> 25,
                                       "26" -> 26, "27" -> 27, "28" -> 28, "29" -> 29, "30" -> 30, "31" -> 31,
                                       "32" -> 32, "33" -> 33, "34" -> 34, "35" -> 35, "36" -> 36)


  //Level 0
  val spinResults : Var[Seq[String]] = Var(Seq.empty[String])

  //Level 1
  val lastWinNumber = Rx(spinResults().headOption)
  val doubleZeroWheelspinHits = Rx(spinResults().take(maxSpins)
    .foldLeft(emptyspinHitsMap) {
      case (result,"") => result
      case (result,x) => {
        val current = result.get(x)
        result.updated(x,current.getOrElse(0)+1)
        }
      }
  )

  val spinCount = Rx(spinResults().take(maxSpins).length)
  val zeroCount = Rx(spinResults().take(maxSpins).count(x => {numberMap(x) == 0}))
  val evenCount = Rx(spinResults().take(maxSpins).count(x => {(numberMap(x)%2 == 0) & (numberMap(x) != 0)}))
  val oddCount = Rx(spinResults().take(maxSpins).count(x => {numberMap(x)%2 != 0}))
  val redCount = Rx(spinResults().take(maxSpins).count(x => {redNumbers.contains(numberMap(x))}))
  val blackCount = Rx(spinResults().take(maxSpins).count(x => {blackNumbers.contains(numberMap(x))}))
  val oneTo18Count = Rx(spinResults().take(maxSpins).count(x => {1 until 18 contains numberMap(x)}))
  val eighteenTo36Count = Rx(spinResults().take(maxSpins).count(x => {18 until 36 contains numberMap(x)}))
  
  //Level 2
  val oddPercentage = Rx((100*oddCount())/spinCount())
  val evenPercentage = Rx((100*evenCount())/spinCount())
  val zeroPercentage = Rx((100*zeroCount())/spinCount())

  val redPercentage = Rx((100*redCount())/spinCount())
  val blackPercentage = Rx((100*blackCount())/spinCount())

  val hot4 = Rx(doubleZeroWheelspinHits().toSeq.sortBy(_._2).takeRight(4))
  val cold4 = Rx(doubleZeroWheelspinHits().toSeq.sortBy(_._2).take(4))

  spinResults.triggerLater{
    println("-->" + doubleZeroWheelspinHits)
  }
  
  spinResults() = Seq(" 1")

  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 2"," 1")

  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 3"," 2"," 1")

  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 3"," 3"," 2"," 1")

  
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)

  spinResults() = Seq(" 3"," 3"," 3"," 2"," 1")

  
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq(" 0"," 3"," 3"," 3"," 2"," 1")

  
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)


  spinResults() = Seq("00"," 0"," 3"," 3"," 3"," 2"," 1")

  
  println("Hot List= " + hot4)
  println("Cold List= " + cold4)



}
