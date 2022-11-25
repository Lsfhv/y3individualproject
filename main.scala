abstract class State
case object Q0 extends State
case object Q1 extends State
case object Q2 extends State
case object Q3 extends State
case object Q4 extends State

val states: List[State] = Q0::Q1::Q2::Nil
val finalStates: List[State] = Q1::Nil
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
    case (Q0, '1') => Q1
    case (Q0, '0') => Q2
    case (Q2, _) => Q2
    case (Q1, _) => Q1
}

val dfa = new DFA(states, Q0, transition, finalStates)

// minimize the DFA

// calcualte the transiton monoid

// check if its aperiodic