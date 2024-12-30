import java.util.regex.Pattern
import scala.util.matching.Regex.MatchIterator
import scala.collection.mutable.ArrayBuffer
import java.util.IllegalFormatException
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import Console.{RESET, GREEN, BOLD, MAGENTA}

enum Token:
  case Value(n: BigDecimal)
  case Operand(op: Char)

def tokenize(arg: String): List[Token] =
  val pattern = raw"(\d*\.?\d+)|([+\-/*%()])".r
  pattern
    .findAllMatchIn(arg)
    .map { m =>
      (m.group(1), m.group(2)) match
        case (n, null)  => Token.Value(BigDecimal(n))
        case (null, op) => Token.Operand(op.charAt(0))
        case _ =>
          throw new IllegalArgumentException(
            "Errorneous match"
          )
    }
    .toList

def precedence(op: Char): Int =
  op match
    case '-' => 1
    case '+' => 2
    case '*' => 3
    case '/' => 4
    case '%' => 5
    case _   => 0

def toRPN(tokens: List[Token]): ArrayBuffer[Token] =
  var holdingStack = List[Char]()
  var output = ArrayBuffer[Token]()
  for (token <- tokens) do
    token match
      case Token.Value(_) => output.append(token)
      case Token.Operand('(') =>
        holdingStack = '(' +: holdingStack
      case Token.Operand(')') =>
        while holdingStack.nonEmpty && holdingStack.head != '(' do
          output.append(Token.Operand(holdingStack.head))
          holdingStack = holdingStack.tail
        if holdingStack.nonEmpty && holdingStack.head == '(' then
          holdingStack = holdingStack.tail
      case Token.Operand(op) =>
        while holdingStack.nonEmpty && precedence(
            holdingStack.head
          ) >= precedence(op)
        do
          output.append(Token.Operand(holdingStack.head))
          holdingStack = holdingStack.tail
        holdingStack = op +: holdingStack

  while holdingStack.nonEmpty do
    output.append(Token.Operand(holdingStack.head))
    holdingStack = holdingStack.tail
  output

def apply(lhs: BigDecimal, rhs: BigDecimal, op: Char): BigDecimal =
  op match
    case '+' => lhs + rhs
    case '-' => lhs - rhs
    case '*' => lhs * rhs
    case '/' => lhs / rhs
    case _   => throw new IllegalArgumentException("Unrecognized operator")

def apply_percentage(
    lhs: BigDecimal,
    rhs: BigDecimal,
    secondaryOp: Token
): BigDecimal =
  secondaryOp match
    case Token.Operand(op) => apply(lhs, rhs * lhs / 100, op)
    case _ => throw new IllegalArgumentException()

def evaluateRPN(tokens: ArrayBuffer[Token]): BigDecimal =
  var solvingStack = List[BigDecimal]()
  var i = 0
  while i < tokens.length do
    val token = tokens(i)
    token match
      case Token.Value(n) => solvingStack = n +: solvingStack
      case Token.Operand(op) if op == '%' =>
        val secondary = tokens(i + 1)
        solvingStack match
          case rhs :: lhs :: rest =>
            val result = apply_percentage(lhs, rhs, tokens(i + 1))
            solvingStack = result +: rest
            i += 1
          case _ =>
            throw new IllegalArgumentException(
              "Malformed RPN <percentage operands>"
            )
      case Token.Operand(op) =>
        solvingStack match
          case rhs :: lhs :: rest =>
            val result = apply(lhs, rhs, op)
            solvingStack = result +: rest
          case _ =>
            throw new IllegalArgumentException("Malformed RPN <operands>")
    i += 1
  solvingStack.head

def evaluate(expr: String): BigDecimal = 
  evaluateRPN(toRPN(tokenize(expr)))  

@main def main(): Unit =
  println("Math Expression Calculator (type 'exit' to quit)")
  while true do
    try
      Console.print(">>> ")
      val input = StdIn.readLine().trim
      if input.toLowerCase == "exit" then
        Console.println("Goodbye!")
        return
      val result = evaluate(input)
      Console.println(s"${RESET}${GREEN}${BOLD}>>> ${result}${RESET}")
    catch
      case NonFatal(e) =>
        Console.println(s"${RESET}${MAGENTA}Error : $e${RESET}")
