import scala.util._
import scala.io._
import java.util.StringTokenizer
import scala.collection.immutable.StringOps
import scala.collection.immutable.Vector
import scala.collection.mutable.ListMap

object Parser{
	import Polynomial._

	// These 2 vars are filled in by parseFile method below.
	var allVariables = null : Vector[String]
	var allPolynomials = null : Vector[Pol]
	
	// The desired signs of the polynomials compared to 0: > , < , >=, <= , =
	var desiredSignsOfPolynomials = Vector.empty : Vector[String]
	
	def allVars : Vector[String] = allVariables
	def allPolys : Vector[Pol] = allPolynomials
	
	// Parse method - returns a pair - (list of all variables, list of polynomials)
	def parseFile(filename : String) : (Vector[String], Vector[Pol]) = {
	    allVariables = null : Vector[String]
	    allPolynomials = null : Vector[Pol]
	    desiredSignsOfPolynomials = Vector.empty : Vector[String]
	    
		var lines = scala.io.Source.fromFile(filename, "utf-8").getLines.toList
		var vars = Nil : List[String]
		
		// Parse each line of the input file.
		for (line1 <- lines){
			var line = ""
			if(line1.size > 0 && line1.charAt(0) != '%'){
			val pp = line1.split(" ")
			for (xx <- pp)
			  line = line + xx
			val st = new StringTokenizer(line, "<>=")
			if(!st.hasMoreElements){
			  throw new Exception("Input polynomial has invalid format: " + line)
			}
			var pol = st.nextToken
			if(!st.hasMoreElements){
			  throw new Exception("Input polynomial has invalid format: " + line)
			}
			
			// Find list of all variables first :
			val sto = new StringTokenizer(pol, "*+-^")
			while(sto.hasMoreTokens){
				var tok = sto.nextToken
				try{
				  tok.toDouble
				} catch { case e:Exception =>
					if(!vars.contains(tok))
					  vars = vars ::: List(tok)
				}
			}	
			}
		}
		
		println("Parsing succeded. Results:")
		allVariables = Vector.empty
		for ( v <- vars)
			allVariables = allVariables :+ v
		println("List of all variables:\n" + allVars)

		// Parse all polynomials:
		allPolynomials = Vector.empty
		for (line1 <- lines){
			var line = ""
			if(line1.size > 0 && line1.charAt(0) != '%'){  
			val pp = line1.split(" ")
			for (xx <- pp)
			  line = line + xx
			  
			var sign = ""
			for(i <- (1 to line.size - 1))
			  if(line.charAt(i) == '<' || line.charAt(i) == '>' || line.charAt(i) == '=')
			    sign = sign + line.charAt(i)
			desiredSignsOfPolynomials = desiredSignsOfPolynomials :+ sign
			val st = new StringTokenizer(line, "<>=")
			allPolynomials = allPolynomials :+ newpol(st.nextToken, allVariables)
			}
		}
		println("List of all polynomials:\n" + allPolys + "\n")
		println("List of desired signs : " + desiredSignsOfPolynomials)
		(allVars, allPolynomials)
	}
  
	// Parses the input String that represents a factor (no + or -) and returns a valid Pol object:
	def parseFactor(s : String, vars : Vector[String]) : Pol = {
		if(vars.size == 0){
		  if(s.size == 0)
		    return new Constant(1)
		  return new Constant(s.toDouble)
		}
		var newvars = vars.drop(1)
		var news = ""
		val sttimes = s.split("\\*")
		var deg = 0
		for (ftimes <- sttimes){
		  val t = ftimes.split("\\^")
		  if(t(0).compareTo(vars(0)) == 0){
		    if(t.size == 1)
		      deg = 1
		    else
		      deg = t(1).toInt
		    
		  } else {
		    news = news + ftimes + "*"
		  }
		}
		if(news.size > 0)
			news = news.substring(0, news.size - 1)
		var map = new ListMap[Int, Pol]
		map.update(deg, parseFactor(news, newvars))		
		new Poly(vars(0), deg, map)
	}
	

	// Parses the input String and returns a valid Pol object:
	def newpol(s : String, vars : Vector[String]) : Pol = {
		var line = ""
		val pp = s.split(" ")
		for (xx <- pp)
		  line = line + xx	  
		var coef = Vector.empty : Vector[Poly]
		var xn = vars(0)
		var result = parseFactor("0", vars)
		val stplus = line.split("\\+")
		for (fplus <- stplus){
		  val stminus = fplus.split("-")
		  var first = stminus(0)
		  if(first.size == 0)
		    first = "0"
		  var interm = parseFactor(first, vars)
		  for(i <- (1 to stminus.size - 1)) {
		    interm = interm - parseFactor(stminus(i), vars)
		  }
		  result = result + interm
		}
		result
	}	  
}