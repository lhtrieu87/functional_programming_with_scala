object NewtonSquareRootSolver {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def squareRootIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else squareRootIter(improve(guess, x), x)
  }                                               //> squareRootIter: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double) = {
    // Divide by x to normalize as for the cases in which
    // x is too large (non terminated) or too small (imprecise result)
    abs(guess * guess - x) / x < 0.001
  }                                               //> isGoodEnough: (guess: Double, x: Double)Boolean

  def improve(guess: Double, x: Double) = {
    (x / guess + guess) / 2
  }                                               //> improve: (guess: Double, x: Double)Double

  def squareRoot(x: Double) = {
    if (x < 0) throw new IllegalArgumentException("Input must be a positive number!")
    if (x == 0) 0
    else
      squareRootIter(1, x)
  }                                               //> squareRoot: (x: Double)Double

  squareRoot(4)                                   //> res0: Double = 2.000609756097561
  squareRoot(1e-10)                               //> res1: Double = 1.0000558643074985E-5
  squareRoot(1e10)                                //> res2: Double = 100005.58643074983
  squareRoot(0)                                   //> res3: Double = 0.0
  squareRoot(-4)                                  //> java.lang.IllegalArgumentException: Input must be a positive number!
                                                  //| 	at NewtonSquareRootSolver$$anonfun$main$1.squareRoot$1(NewtonSquareRootS
                                                  //| olver.scala:20)
                                                  //| 	at NewtonSquareRootSolver$$anonfun$main$1.apply$mcV$sp(NewtonSquareRootS
                                                  //| olver.scala:30)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at NewtonSquareRootSolver$.main(NewtonSquareRootSolver.scala:1)
                                                  //| 	at NewtonSquareRootSolver.main(NewtonSquareRootSolver.scala)
}