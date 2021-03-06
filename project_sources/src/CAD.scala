import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

// Extension phase of the algorithm
object CAD {
	import Parser._;
  	import Polynomial._
  	import Utilities._
  	import ProjectionPhase._
  	import Univariate._
  	import BasePhase._
  	
  	// Function that takes a set of univariate polynomials and a ListMap of already assigned values 
  	// to variables (a partial cell) and builds a new set of cells
  	def ExtensionPhaseSingle(polys : Vector[Pol], partialCell : ListMap[String, Double]) : Vector[ListMap[String, Double]] ={
  	  var evaluatedPolys = Vector.empty[Pol]
  	  for(p <- polys){
  	    var aux = p
  	    for(i <- (1 to partialCell.size))
  	      aux = aux.subst(partialCell.get(lastVar(aux)).get)
  	    evaluatedPolys = evaluatedPolys :+ aux
  	  }

  	  var samples = samplePoints(evaluatedPolys)
  	  var result = Vector.empty[ListMap[String, Double]]
  	  for(i <- (0 to samples.size - 1)){
  	    var newCell = new ListMap[String, Double]
  	    for((s,d) <- partialCell)
  	      newCell.update(s,d)
  	    newCell.update(mainVar(evaluatedPolys(0)), samples(i))
  	    result = result :+ newCell
  	  }
  	  result
  	}

  	// Function that takes a set of polynomials and a set of partial cells of already assigned values 
  	// to some of the variables and builds a new set of cells  	
  	def ExtensionPhase(polys : Vector[Pol], partialCells : Vector[ListMap[String, Double]]) : Vector[ListMap[String, Double]] ={
  	  var result = Vector.empty[ListMap[String, Double]]
  	  for(i <- (0 to partialCells.size - 1)){
  	    result = result ++ ExtensionPhaseSingle(polys, partialCells(i))
  	  }
  	  result
  	}
  	
  	
  	// Splits R^n in cells based on the given set of polynomials. 
  	// Returns sample points from each cell of R^n space.
  	def cells(polys : Vector[Pol]) : Vector[ListMap[String, Double]] = {
  	  var projs = projectionToUnivariatePolys(polys)._2 // projections(0) = initial polys
  	  var allCells = Vector.empty[ListMap[String, Double]] :+ (new ListMap[String, Double])

  	  for(i <- (0 to projs.size - 1)){
  	    allCells = ExtensionPhase(projs(projs.size - i - 1), allCells)
  	  }
  	  allCells
  	}
  	
  	// Main function: CHOOSE function - returns a valid assign to all variables if it exists or null if 
  	// there is no valid assignment for variables as to satisfy the desired signs ( < 0, = 0, etc)
  	def choose(polys : Vector[Pol], desiredSigns : Vector[String]) : ListMap[String, Double] = {
  	  var Cells = cells(polys)
  	  for(c <- Cells){
  	    var goodAssignment = true
  	    for(i <- (0 to polys.size - 1)){
  	      var aux = polys(i)
  	      for(i <- (1 to c.size))
  	        aux = aux.subst(c.get(lastVar(aux)).get)
  	      var value = aux.value
  	      if(desiredSigns(i).compareTo("=") == 0 && Math.abs(value) > 3*Main.eps)
  	        goodAssignment = false
  	      if(desiredSigns(i).compareTo(">") == 0 && value < Main.eps/3)
  	        goodAssignment = false
  	      if(desiredSigns(i).compareTo("<") == 0 && value > -Main.eps/3)
  	        goodAssignment = false
  	      if(desiredSigns(i).compareTo(">=") == 0 && value < -Main.eps/3)
  	        goodAssignment = false
  	      if(desiredSigns(i).compareTo("<=") == 0 && value > -Main.eps/3)
  	        goodAssignment = false
  	    }
  	    if(goodAssignment)
  	      return c
  	  }
  	  null
  	}
  	
}