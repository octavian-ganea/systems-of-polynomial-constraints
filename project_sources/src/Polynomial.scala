import scala.collection.immutable.Vector
import scala.io._
import java.util.StringTokenizer
import scala.collection.immutable.StringOps
import scala.collection.mutable.ListMap

object Polynomial{
	import Parser._
	import Utilities._
	import Univariate._
	
	// Complex operation: not to be called to often.
	// simplify 0 coefficients of a polynomial:
	def simplify(p : Pol) : Pol = {
		p match {
		  case Constant(d) => p
		  case Poly(v, deg, coefs) => {
		    var cs = new ListMap[Int, Pol]
		    var newdeg = 0
		    for((i,coef) <- coefs){
		      var newcoef = simplify(coef)
		      if(!newcoef.isZero){
		    	  cs.update(i, newcoef)
		    	  if(newdeg < i)
		    	    newdeg = i
		      }
		    }
		    if(cs.isEmpty)
		      cs.update(0, simplify(coefs.get(deg).get))
		    new Poly(v, newdeg, cs)
		  }
		}
	}	

	def isPoly(p : Pol) : Boolean = {
	  p match {
	    case Constant(_) => false
	    case _ => true
	  }
	}

	def mainVar(p : Pol) : String = {
	  p match {
	    case Constant(_) => null
	    case Poly(v, _, _) => v
	  }
	}

	def lastVar(p : Pol) : String = {
	  p match {
	    case Constant(_) => null
	    case Poly(v, deg, coefs) => {
	      coefs.get(deg).get match{
	        case Constant(d) => v
	        case x => lastVar(x)
	      }
	    }
	  }
	}
	
	// Polynomial class represented by one variable and coefficients that are also
    // polynomials but with one less variable. 
	// In case there are no variables, the polynomial is a double constant.
    sealed abstract class Pol{
      def coefs : ListMap[Int, Pol]
      def isConstant : Boolean
      def deg : Int
      def eequals(p : Pol) : Boolean
      // We assume that the following operations are only between polynomials with the same set of variables.
	  // That is, if we add 2 polynomials, they must have the same Variable and their coefficients must be 
      // recursively with the same set of variables.
      def unary_- : Pol
      def +(p : Pol) : Pol
      def -(p : Pol) : Pol = {
        this +(-p)
      }
      def *(p : Pol) : Pol
      def isZero : Boolean
      //Creates a zero pol using the set of variables defined by this polynomial
      def createZeroPol() : Pol
      
      // Takes the reductum of the given polynomial until degree k (including k)
      // The reductum is taken with respective to the current main variable
      // For example, "x^2 y + x y^2 + 1".reductum(1) = "x y^2 + 1" (taken in raport with variable x) 
      def reductum(k : Int) : Pol

      def timesCt(c : Double) : Pol
      def derivative() : Pol    
      
      // Substitution of x instead of the last variable in the polynomial.
      // Last variable is the one of the subpolynomial that has all its coefficients constants.
      def subst(x : Double) : Pol
      
      // Returns value of the polynomial if it is a constant. 
      // Throws an exception if it is not a constant.
      def value : Double
    }
    
    case class Constant(val d: Double) extends Pol {
      def eequals(p : Pol) : Boolean = {
        p match {
          case Constant(e) => return Math.abs(e - d) < Main.eps
          case _ => throw new Exception("Comparison between 2 polys with different vars: " + p + " ; " + this)
        }       
      }
      
      def isConstant : Boolean = true
      
      def coefs : ListMap[Int, Pol] = {
        var v = new ListMap[Int,Pol]
        v.update(0, this)
        v
      }

      def deg : Int = 0
      
      def isZero : Boolean = {
    	d == 0
      }
      def unary_- : Pol = {
        new Constant(-d)
      }
      def +(p : Pol) : Pol = {
        p match {
          case Constant(e) => new Constant(e + d)
          case _ => throw new Exception("Addition between 2 polys with different vars: " + p + " ; " + this)
        }
      }
      def *(p : Pol) : Pol = {
        p match {
          case Constant(e) => new Constant(e * d)
          case _ => throw new Exception("Multiplication between 2 polys with different vars: " + p + " ; " + this)
        }
      }
      override def toString: String = {
        d + ""
      }
      def createZeroPol() : Pol = new Constant(0)
      def reductum(k : Int) : Pol = this
      def timesCt(c : Double) : Pol = new Constant(d*c)
      def derivative() : Pol = new Constant(0)
      def subst(x : Double) : Pol = throw new Exception("Substitution on a constant polynomial: " + this)
      def value : Double = d
    }
    
    case class Poly(val Var:String, val degree:Int, val Coefs:ListMap[Int, Pol]) extends Pol{
      def coefs : ListMap[Int, Pol] = {
        Coefs
      }
      
      def isConstant : Boolean = {
        if(degree > 0)
          return false
        return coefs.get(0).get.isConstant
      }
      
      def deg : Int = degree
      
      override def toString: String = {
        if(degree == 0)
          return coefs.get(0).get.toString
        var res = ""
        for((i,p) <- coefs){
            var power = Var + "^" + i
            if(i == 0)
              power = ""
            if(i == 1)
              power = Var
       	    try{
       		  p.toString.toDouble
              res = p.toString + " " +  power + " + " + res
       	    } catch { case e:Exception =>
              res = "(" + p.toString + ")" + power + " + " + res
       	    }            
        }
        if(res.length >= 3)
        	return res.substring(0, res.length - 3)
        res
      }    	

      def isZero : Boolean = {
        for(f <- coefs)
        	if(!f._2.isZero)
        	  return false
        return true
      }

      def eequals(p : Pol) : Boolean = {
    	p match {
          case Poly(varp, degp, coefsp) => {
            if(varp.compareTo(Var)!= 0){
              throw new Exception("Comparison between 2 polys with different vars: " + p + " ; " + this)
            }
            for((i1, p1) <- coefs){
              if(!coefsp.contains(i1) || !p1.equals(coefsp.get(i1).get))
                return false
            }
            for((i1, p1) <- coefsp){
              if(!coefs.contains(i1) || !p1.equals(coefs.get(i1).get))
                return false
            }
            
            return true
          }
          case _ => throw new Exception("Comparison between 2 polys with different types: " + p + " ; " + this)
        }    	
      }
      
      def unary_- : Pol = {
        var hash = new ListMap[Int, Pol]
        for(f <- coefs)
          hash.update(f._1, -f._2)
    	return new Poly(Var, degree, hash)
      }
    	
      def +(p : Pol) : Pol = {
    	p match {
          case Poly(varp, degp, coefsp) => {
            if(varp.compareTo(Var)!= 0){
              throw new Exception("Addition between 2 polys with different vars: " + p + " ; " + this)
            }
            var hash = new ListMap[Int, Pol]
            var degResult = Math.max(degree, degp)
            
            for((i1, p1) <- coefs)
              hash.update(i1, p1)
            for((i2, p2) <- coefsp)
              if(hash.contains(i2))
                hash.update(i2, p2 + hash.get(i2).get)
              else
                hash.update(i2, p2)

            simplify(new Poly(varp, degResult, hash))
          }
          case _ => throw new Exception("Addition between 2 polys with different types: " + p + " ; " + this)
        }    	
      }
      
      def *(p : Pol) : Pol = {
    	p match {
          case Poly(varp, degp, coefsp) => {
            if(varp.compareTo(Var)!= 0){
              throw new Exception("Multiplication between 2 polys with different vars: " + p + " ; " + this)
            }
            var hash = new ListMap[Int, Pol]
            var degResult = degree + degp
            for((i1, p1) <- coefs)
              for((i2, p2) <- coefsp)
                if(hash.contains(i1 + i2))
                  hash.update(i1 + i2, hash.get(i1 + i2).get + (p1 * p2))
                else
                  hash.update(i1 + i2, p1 * p2)
            simplify(Poly(varp, degResult, hash))
          }
          case _ => throw new Exception("Multiplication between 2 polys with different types: " + p + " ; " + this)
        }        	
      }

      def createZeroPol() : Pol = {
        var h = new ListMap[Int, Pol]
        h.update(0,coefs.get(degree).get.createZeroPol)
        new Poly(Var, 0, h)
      }
      
      def reductum(k : Int) : Pol = {
        var hash = new ListMap[Int, Pol]
        var degResult = 0
        for((i,p) <- coefs)
          if(i <= k){
            hash.update(i,p)
            if(i > degResult)
              degResult = i
          }
        if(hash.isEmpty)
          return this.createZeroPol
        new Poly(Var, degResult, hash)
      }

      def timesCt(c : Double) : Pol = {
        var hash = new ListMap[Int, Pol]
        var degResult = degree
        for((i,p) <- coefs)
          hash.update(i, coefs.get(i).get.timesCt(c))
        if(hash.isEmpty)
          return this.createZeroPol
        new Poly(Var, degResult, hash)        
      }
      
      def derivative() : Pol = {
        var hash = new ListMap[Int, Pol]
        var degResult = degree - 1
        // println(coefs.toList + " " + Var)
        for((i,p) <- coefs)
          if ( i > 0){
            // println(coefs.get(i).get.timesCt(i))
            hash.update(i - 1, coefs.get(i).get.timesCt(i))
          }
        if(hash.isEmpty)
          return this.createZeroPol
        new Poly(Var, degResult, hash) 
      }
      
      def value : Double = throw new Exception("This polynomial does not have a value; it is not constant: " + this)
      
      def subst(x : Double) : Pol = {
    	coefs.get(degree).get match {
          case Poly(_, _ , _) => {
            var hash = new ListMap[Int, Pol]
            for((i, p) <- coefs)
              hash.update(i, p.subst(x))
            return new Poly(Var, degree, hash)
          }
          case Constant(d) => {
            var eval : Double = 0            
  /*          for((i,p) <- coefs)
              eval = eval + p.value * Math.pow(x, i)
*/
           for(i <- (0 to degree))
              if(coefs.contains(degree - i))
                eval = eval*x + coefs.get(degree - i).get.value
              else
                eval = eval * x
                
            new Constant(eval)
          }
        }       
      }
    }
    
    
    
    def PolyTest : Unit = {    
      	var p = newpol("0.921599999999998  + 21.28 * z^2 -8.44799999999999 * z - 8.8 * z^3 + z^4", 
      	    Vector.empty[String] :+ "z")
		println(allRoots(p))
      
    }
}