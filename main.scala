abstract class State
case object Q0 extends State
case object Q1 extends State
case object Q2 extends State
case object Q3 extends State
case object Q4 extends State

val states: List[State] = Q0::Q1::Q2::Q3::Q4::Nil
val finalStates: List[State] = Q4::Nil
val alphabet: List[Char] = '1'::'0'::Nil

case class DFA(states: List[State],
    start: State, 
    transition: (State, Char) => State, 
    finalStates: List[State]) {

        def compute(q: State, input: List[Char]): State = input match {
            case Nil => q
            case x::xs => compute(transition(q,x),xs)
        }

        def accepts(s: String) : Boolean = {
            finalStates.contains(compute(start, s.toList))
        }
    }

// example dfa, string starting with 1

val transition : (State, Char) => State = {
    case (Q0, '1') => Q2
    case (Q0, '0') => Q1
    case (Q1, '0') => Q4
    case (Q1, '1') => Q2
    case (Q2, '0') => Q3
    case (Q2, '1') => Q2
    case (Q3, '0') => Q4
    case (Q3, '1') => Q0
    case (Q4, _) => Q4
}

val dfa = new DFA(states, Q0, transition, finalStates)

// First phase 
// DFA minimization by brzozwski

implicit class Crossable[X](xs: List[X]) {
    def cross[Y](ys: List[Y]) = 
        (for (i <- 0 until xs.size) yield 
            for (s <- (for (j <- i+1 until ys.size) yield ys(j)).toList) yield (xs(i), s)).toList.flatten.toSet
}

def initialMarked(q: State, p: State, marked: Set[(State, State)]) = {
    if ((finalStates.contains(q) && !finalStates.contains(p)) || 
        (!finalStates.contains(q)) && finalStates.contains(p) || 
        marked.contains((q,p)) || marked.contains((p,q))
        ) true else false
} // check if recheable by transition to a marked state now.

def reachable(q: State, p: State, marked: Set[(State, State)]) = {
    var flag = false
    for (x <- alphabet) yield{
        if (marked.contains(transition(q, x), transition(p, x)) || marked.contains(transition(p, x), transition(q, x))) {
            flag = true
        } 
    }
    flag
}

def recurse(marked: Set[(State, State)], unmarked: Set[(State, State)]): (Set[(State, State)], Set[(State, State)]) = {
    val x = unmarked.partition(x=>initialMarked(x._1, x._2, marked))
    if (x._1.size == 0) (marked, unmarked) else recurse(marked ++ x._1, x._2)
}

def main(marked: Set[(State, State)], unmarked: Set[(State, State)]): (Set[(State, State)], Set[(State, State)]) = {
    val x = unmarked.partition(x=>reachable(x._1, x._2, marked))
    if (x._1.size == 0) (marked, unmarked) else main(marked ++ x._1, x._2)
}

val x = recurse(Nil.toSet, states cross states)
main(x._1, x._2)

// now minimize