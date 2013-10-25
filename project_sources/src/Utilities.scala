import scala.collection.immutable.Vector
import scala.io._
import java.util.StringTokenizer
import scala.collection.immutable.StringOps
import scala.collection.mutable.ListMap

object Utilities {
	import Polynomial._

	def printVector(v : Vector[Pol]) : Unit = {
	  for(e <- v)
	    println(e)
	}
	
	// Takes a NxN matrix with polynomial entries and computes its determinant as a polynomial.
	def determinant(a : Array[Array[Pol]]) : Pol = {
	  if(a.size == 0)
	    throw new Exception("Array of size 0")
	  if(a.size == 1)
	    return a(0)(0)
	  var result = null : Pol
	  var constant = 1 : Double
      for(j <- (0 to a.length - 1)) {
        var temp = Array.ofDim[Pol](a.length - 1, a.length - 1)
        for(j1 <- (0 to j - 1))
          for(i <- (1 to a.length - 1))
            temp(i - 1)(j1) = a(i)(j1)
        for(j1 <- (j + 1 to a.length - 1))
          for(i <- (1 to a.length - 1))
            temp(i - 1)(j1 - 1) = a(i)(j1)
              
        if(result == null)
          result = determinant(temp) * (a(0)(j).timesCt(constant))
        else
          result = result + (determinant(temp) * (a(0)(j).timesCt(constant)))
        constant = -constant
      }
	  result
	}

	// PSC function from standard CAD algorithm. 
	// psc_k(f,g) is the dominant coefficient of subres_k(f,g)
	def psc(k : Int, f : Pol, g : Pol) : Pol = {
	  f match {
	    case Constant(d) => throw new Exception("Trying to compute PSC for constant polynomials" + f + " " + g)
	    case Poly(vf, m, coefsf) => {
	      g match {
	        case Constant(d) => throw new Exception("Trying to compute PSC for constant polynomials" + f +" "+ g)
	        case Poly(vg, n, coefsg) => {
	          if(vf.compareTo(vg) != 0)
	            throw new Exception("Trying to compute PSC for different vars" + vf +" "+ vg)
	          var zeropol = coefsf.get(m).get.createZeroPol
	          if(n + m - 2*k <= 0)
	            return zeropol
	          var a = Array.ofDim[Pol](n + m - 2*k, n + m - 2*k)
	          for(i <- (0 to n - k - 1)){
	            for(j <- (0 to i - 1))
	              a(i)(j) = zeropol
	            for(j <- (i to n + m - 2*k - 1))
                  if(coefsf.contains(m + i - j))
                    a(i)(j) = coefsf.get(m + i - j).get
                  else
                    a(i)(j) = zeropol
	          }
	          for(i <- (0 to m - k - 1)){
	            for(j <- (0 to i - 1))
	              a(i + n - k)(j) = zeropol
	            for(j <- (i to n + m - 2*k - 1))
                  if(coefsg.contains(n + i - j))
                    a(i + n - k)(j) = coefsg.get(n + i - j).get
                  else
                    a(i + n - k)(j) = zeropol
	          }
	          if(a.size > 0){ 
	        	  return determinant(a)
	          }
	          return zeropol
	        }
	      }
	    }
	  }
	  null
	}
	

}