import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

// Base phase of the algorithm
object BasePhase {
	import Parser._;
  	import Polynomial._
  	import Utilities._
  	import ProjectionPhase._
  	import Univariate._
  	
  	// Returns univariate polynomials (proj^{n-1}) and a set of all projections including the last univariate ones
  	// that are returned as the first result.
  	def projectionToUnivariatePolys(v : Vector[Pol]) : (Vector[Pol], Vector[Vector[Pol]]) = {
  	  var allProjections = Vector.empty[Vector[Pol]] :+ v
  	  var res = v
  	  if(v.size == 0)
  	    return (v, allProjections)
  	  while(isPoly(res(0).coefs.get(res(0).deg).get)){
  	    res = projection(res)
  	    allProjections = allProjections :+ res  	    
  	  }
  	  (res, allProjections)
  	}
  	
  	
  	// Gives sample points from a set of univariate polynomials.
  	// These points are roots and points between roots of all polynomials
  	def samplePoints(v : Vector[Pol]) : Vector[Double] = {
  	  if(v.size == 0)
  	    return null

  	  var vector = allRoots(v(0))
  	  for(i <- (1 to v.size - 1)){
  	    vector = vector ++ allRoots(v(i))  	    
  	  }

  	  vector = vector.sortWith((a,b) => (a < b))
  	  // Remove duplicates from sample points
  	  var aux = Vector.empty[Double]
  	  for(i <- (0 to vector.size - 2)){
  	    if(Math.abs(vector(i) - vector(i + 1)) > 0.001){
  	      aux = aux :+ vector(i)
  		}
  	  }
  	  
  	  if(vector.size > 0)
        aux = aux :+ vector(vector.size - 1)

  	  var result = Vector.empty[Double] 
  	  if(aux.size > 0)
  	    result = result :+ (aux(0) - 1)
  	  else
  	    return result :+ 0.0
  	  for(i <- (0 to aux.size - 2)){
  	    result = result :+ aux(i)
  	    result = result :+ (aux(i)/2 + aux(i+1)/2)  	    
  	  }
  	  result = result :+ aux(aux.size - 1)  	  
  	  result = result :+ (aux(aux.size - 1) + 1)  	  
  	  result
  	}
  	


}