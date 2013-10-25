import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

object Main {
	import Parser._;
  	import Polynomial._
  	import Utilities._
  	import ProjectionPhase._
  	import Univariate._
  	import BasePhase._
  	import CAD._
  	
  	// Used for equality checks involving values of polynomials in some points
  	// Roots of polys are found within an error of eps * 10000
  	def eps = 0.000000000001 : Double
  
  	def testFile(f : String) : Unit = {
		println("Parsing file " + f)
		parseFile(f)
  	  
		println("Analysed polynomials: ")
		printVector(allPolynomials)		
		println("vars: " + allVariables)
	
		println("\nUnivariate projection (on real axis) contains these polynomials: ")		
		printVector(projectionToUnivariatePolys(allPolynomials)._1)
		println()
		
		var cells = CAD.cells(allPolynomials)
		println("There are " + cells.size + " cells")		
		for(c <- cells)
		  println(c)
		var result = choose(allPolynomials, desiredSignsOfPolynomials)		  
		println("====== FINAL RESULT FOR " + f + " ================================")
		println(result)  	  
  	}
  	
	def main(args : Array[String]) : Unit = {
  		println("====================================================")
//    	testFile("tests/in.txt")		
//		testFile("tests/test1")
//		testFile("tests/test2")
//		testFile("tests/test3")
//		testFile("tests/test4")
//		testFile("tests/test5")
//		testFile("tests/test6") // NULL; see comments in the test file
		testFile("tests/test7")
	}
}


















