

val states: List[String] = "Q0"::"Q1"::"Q2"::"Q3"::"Q4"::Nil
val finalStates: List[String] = "Q4"::Nil
val alphabet: List[Char] = '0'::'1'::Nil


val transition : (String, Char) => String = {
    case ("Q0", '1') => "Q2"
    case ("Q0", '0') => "Q1"
    case ("Q1", '0') => "Q4"
    case ("Q1", '1') => "Q2"
    case ("Q2", '0') => "Q3"
    case ("Q2", '1') => "Q2"
    case ("Q3", '0') => "Q4"
    case ("Q3", '1') => "Q0"
    case ("Q4", _) => "Q4"
}

case class DFA(states: List[String],
    start: String, 
    transition: (String, Char) => String, 
    finalStates: List[String]) {

        def compute(q: String, input: List[Char]): String = input match {
            case Nil => q
            case x::xs => compute(transition(q,x),xs)
        }

        def accepts(s: String) : Boolean = {
            finalStates.contains(compute(start, s.toList))
        }
}

implicit class Crossable[X](xs: List[X]) {
    def cross[Y](ys: List[Y]) = 
        (for (i <- 0 until xs.size) yield 
            for (s <- (for (j <- i+1 until ys.size) yield ys(j)).toList) yield (xs(i), s)).toList.flatten.toSet
}

def initialMarked(q: String, p: String, marked: Set[(String, String)]) = {
    if ((finalStates.contains(q) && !finalStates.contains(p)) || 
        (!finalStates.contains(q)) && finalStates.contains(p) || 
        marked.contains((q,p)) || marked.contains((p,q))
        ) true else false
} // check if recheable by transition to a marked state now.

def recurse(marked: Set[(String, String)], unmarked: Set[(String, String)]): (Set[(String, String)], Set[(String, String)]) = {
    val x = unmarked.partition(x=>initialMarked(x._1, x._2, marked))
    if (x._1.size == 0) (marked, unmarked) else recurse(marked ++ x._1, x._2)
}

def reachable(q: String, p: String, marked: Set[(String, String)]) = {
    var flag = false
    for (x <- alphabet) yield{
        if (marked.contains(transition(q, x), transition(p, x)) || marked.contains(transition(p, x), transition(q, x))) {
            flag = true
        } 
    }
    flag
}

def main(marked: Set[(String, String)], unmarked: Set[(String, String)]): (Set[(String, String)], Set[(String, String)]) = {
    val x = unmarked.partition(x=>reachable(x._1, x._2, marked))
    if (x._1.size == 0) (marked, unmarked) else main(marked ++ x._1, x._2)
}