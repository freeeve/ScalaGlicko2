package com.skeweredrook.scalaglicko2

import org.scalatest.FunSuite
 
class Glicko2Test extends FunSuite { 
  test("test default value constructor") {
    expect(1500.0/Glicko2.Glicko2Conversion) { new Glicko2().rating }
    expect(350.0/Glicko2.Glicko2Conversion) { new Glicko2().rd }
    expect(0.06) { new Glicko2().volatility; }
  }
  
  test("test convert to glicko1") {
    expect(1500.0) { new Glicko2().toGlicko1().rating; }
    expect(350.0) { new Glicko2().toGlicko1().rd; }
    expect(0.06) { new Glicko2().toGlicko1().volatility; }
  }
  
  test("test toString") {
    expect("rating: 1500, rd: 350.00, volatility: 0.060000") {
      new Glicko2().toString();
    }
  }
  
  test("test calculatePlayerRatings") {
    expect("rating: 1464, rd: 151.52, volatility: 0.059999") {
      val example = Glicko2.fromGlicko1(1500.0, 200.0, 0.06);
      val opponents = List((Glicko2.fromGlicko1(1400.0,  30.0, 0.06), 1.0),
                           (Glicko2.fromGlicko1(1550.0, 100.0, 0.06), 0.0),
                           (Glicko2.fromGlicko1(1700.0, 300.0, 0.06), 0.0));
      example.calculateNewRating(opponents).toString;
    }
  }
}