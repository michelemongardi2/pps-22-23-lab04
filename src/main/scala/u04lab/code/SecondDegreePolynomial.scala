package u04lab.code

// Express a second degree polynomial
// Structure: secondDegree * X^2 + firstDegree * X + constant (ax^2 + bx + c)
trait SecondDegreePolynomial:
  def constant: Double
  def firstDegree: Double
  def secondDegree: Double
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial
  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial


object SecondDegreePolynomial:
  def apply(secondDegree: Double, firstDegree: Double, constant: Double): SecondDegreePolynomial =
    SecondDegreePolynomialImpl(secondDegree, firstDegree, constant)
  private case class SecondDegreePolynomialImpl(override val secondDegree: Double,
                                           override val firstDegree: Double,
                                           override val constant: Double) extends SecondDegreePolynomial:
    override def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
      SecondDegreePolynomial(this.secondDegree + polynomial.secondDegree,
        this.firstDegree + polynomial.firstDegree,
        this.constant + polynomial.constant)

    override def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
      SecondDegreePolynomial(this.secondDegree - polynomial.secondDegree,
        this.firstDegree - polynomial.firstDegree,
        this.constant - polynomial.constant)

@main def checkComplex(): Unit =
  val simplePolynomial: SecondDegreePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)
  val sum = simplePolynomial + anotherPolynomial
  println((sum, sum.secondDegree, sum.firstDegree, sum.constant)) // 1.0 * X^2 + 1.0 * X + 3.0
  val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
  println((multipleOperations, multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant)) // 2.0 * X^2 + 1.0 * X + 2.0

  println("Check toString: " + simplePolynomial.toString)
  val pol1 = SecondDegreePolynomial(1.0, 0, 3)
  val pol2 = SecondDegreePolynomial(1.0, 0, 3)
  val equality: Boolean = pol1 == pol2
  println("Check equals: " + equality)

  val pol3: SecondDegreePolynomial = SecondDegreePolynomial.apply(2.0, 3.0, 10)
  println("Polynomial with Apply" + pol3)

/** Hints:
  *   - implement SecondDegreePolynomial with a SecondDegreePolynomialImpl class, similar to PersonImpl in slides
  *   - check that equality and toString do not work
  *   - use a case class SecondDegreePolynomialImpl instead
  *   - check equality and toString now
  */
