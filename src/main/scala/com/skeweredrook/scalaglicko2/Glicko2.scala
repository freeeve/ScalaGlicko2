package com.skeweredrook.scalaglicko2
import scala.Math

// companion singleton (where static values and functions go)
object Glicko2 {
  val Glicko2Conversion:Double = 173.7178;
  // should this be configurable somehow?
  val Tau:Double = 0.3;
  
  // factory method to create glicko2 rating objects from glicko1 ratings and RDs
  def fromGlicko1(rating:Double, rd:Double, volatility:Double) : Glicko2 = {
    new Glicko2(rating / Glicko2.Glicko2Conversion, rd / Glicko2.Glicko2Conversion, volatility);
  }
}
class Glicko2(var rating:Double = 1500.0/Glicko2.Glicko2Conversion, var rd:Double = 350.0/Glicko2.Glicko2Conversion, var volatility:Double = 0.06) {
  // returns a new Glicko2 object with Glicko1 rating conversion applied
  // this is only slightly confusing... but most people are used to the Glicko1 rating range
  def toGlicko1(): Glicko2 = {
    new Glicko2(rating * Glicko2.Glicko2Conversion, rd * Glicko2.Glicko2Conversion, volatility);
  }
  
  // accepts a list of tuples of opponent ratings: Glicko2, and result:Double (result: 0.0=loss, 0.5=draw, 1.0=win)
  def calculateNewRating(opponents : List[(Glicko2, Double)]) : Glicko2 = {
    // step1 - set tau, system volatility constraint
    // tau set by default to 0.3
    
    // step2 - convert to clicko2 scale
    // already in glicko2 scale
    
    // step3 - compute the variance
    // helper function g
    def g(phi: Double) : Double = {
      1.0 / sqrt(1.0 + 3 * pow2(phi) / pow2(Math.Pi))
    }
    // helper function E
    def E(rating:Double, oppRating:Double, oppRD:Double) : Double = {
      1.0 / (1.0 + exp(-g(oppRD) * (rating - oppRating)));
    }
    // run through opponents to calculate the variance v
    def v : Double = {
      var sum : Double = 0.0;
      opponents.foreach { opp =>
        sum += pow2(g(opp._1.rd)) * E(this.rating, opp._1.rating, opp._1.rd) * (1 - E(this.rating, opp._1.rating, opp._1.rd));
      }
      1.0 / sum;
    }
    
    // step4 - compute the delta
    def Δ = {
      var sum : Double = 0.0;
      opponents.foreach { opp =>
        sum += g(opp._1.rd) * (opp._2 - E(this.rating, opp._1.rating, opp._1.rd));
      }
      v * sum;
    }
    
    // step5 - calculate new volatility
    val ε = 0.000001;
    def newVolatility: Double = {
       0.059999;
       def a : Double = {
         ln(pow2(this.volatility));
       }
       
       def f(x:Double) : Double = {
         (exp(x) * (pow2(Δ) - pow2(this.rd) - v - exp(x))) / (2.0 * pow2(pow2(this.rd)+ v + exp(x))) - (x - a) / pow2(Glicko2.Tau);
       }
       
       var A: Double = a;
       var B: Double = if(pow2(Δ) > pow2(this.rd)) {
         ln(pow2(Δ) - pow2(this.rd) - v);
       } else {
         var k = 1;
         while(f(a-k*sqrt(pow2(Glicko2.Tau))) < 0) {
           k += 1;
         }
         a-k*sqrt(pow2(Glicko2.Tau));
       }
       
       var fA = f(A);
       var fB = f(B);
       while(abs(B-A) > ε) {
         var C:Double = A + (A - B) * fA/(fB-fA);
         var fC = f(C);
         if(fC*fB < 0) {
           A = B;
           fA = fB;
         } else {
           fA = fA/2;
         }
         B = C;
         fB = fC;
       }
       
       exp(A/2);
    }    
    
    // step6 - update rating deviation to new pre-rating period value (decay RD)
    def preRatingRD: Double = {
      sqrt(pow2(this.rd) + pow2(newVolatility));
    }
    
    // step7a - calculate new RD
    def newRD: Double = {
      1.0 / sqrt(1.0 / pow2(preRatingRD) + 1.0 / v);
    }
    
    // step7b - calculate new rating
    def newRating: Double = {
      var sum: Double = 0.0;
      opponents.foreach{ opp =>
        sum += g(opp._1.rd) * (opp._2 - E(this.rating, opp._1.rating, opp._1.rd))
      }
      this.rating + pow2(newRD) * sum;
    }
    
    // step8 isn't needed. we store things in Glicko2 scale
    
    new Glicko2(newRating, newRD, newVolatility);
  }
  
  // more concise math functions (aliased)
  def pow2(op:Double) : Double = {
    op*op;
  }
  
  def sqrt(op:Double) : Double = {
    Math.sqrt(op);
  }
  
  def exp(op:Double) : Double = {
    Math.exp(op);
  }
  
  def ln(op: Double) : Double = {
    Math.log(op);
  }
  
  def abs(op: Double) : Double = {
    Math.abs(op);
  }
  // end friendlier looking math functions
  
  override def toString(): String = {
    "rating: %1.0f, rd: %1.2f, volatility: %1.6f".format(rating, rd, volatility); 
  }
}