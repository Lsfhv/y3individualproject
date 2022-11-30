var states: List[String] = "Q0"::"Q1"::"Q2"::"Q3"::"Q4"::Nil
val finalStates: List[String] = "Q4"::Nil
val alphabet: List[Char] = '0'::'1'::Nil

import scala.collection.mutable.HashMap

var transitionMap: HashMap[(String, Char), String]  = HashMap(
    ("Q0", '1') -> "Q2",
    ("Q0", '0') -> "Q1", 
    ("Q1", '0') -> "Q4", 
    ("Q1", '1') -> "Q2",
    ("Q2", '0') -> "Q3",
    ("Q2", '1') -> "Q2",
    ("Q3", '0') -> "Q4",
    ("Q3", '1') -> "Q0",
    ("Q4", '1') -> "Q4", 
    ("Q4", '0') -> "Q4"
) 

case class DFA(states: List[String],
    start: String, 
    transition: HashMap[(String, Char), String], 
    finalStates: List[String]) {

        def compute(q: String, input: List[Char]): String = input match {
            case Nil => q
            case x::xs => compute(transition((q,x)),xs)
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
        if (marked.contains(transitionMap((q, x)), transitionMap((p, x))) || marked.contains(transitionMap((p, x)), transitionMap((q, x)))) {
            flag = true
        } 
    }
    flag
}

def main(marked: Set[(String, String)], unmarked: Set[(String, String)]): (Set[(String, String)], Set[(String, String)]) = {
    val x = unmarked.partition(x=>reachable(x._1, x._2, marked))
    if (x._1.size == 0) (marked, unmarked) else main(marked ++ x._1, x._2)
}

val x = recurse(Nil.toSet, states cross states)
val f = main(x._1, x._2)

def generateStates(states: List[String], unmarked: Set[(String, String)]): List[String] = {
    val x = for (x <- unmarked.toList) yield{
        s"${x._1},${x._2.substring(1)}"
    }
    states ++ x 
}

def deleteUnmarked(states: List[String], unmarked: List[(String, String)]): List[String] = unmarked match{
    case Nil => states
    case x::xs => deleteUnmarked(states diff x._1::x._2::Nil, xs)
}

// f._2 are the states that need to get merged
val nwStates = generateStates(states, f._2)

var newStates = deleteUnmarked(nwStates, f._2.toList)
var unmarked = f._2.toList

def generateNewTransition(unmarked: List[(String, String)]): Unit = unmarked match{
    case Nil => {
        return
    }
    case x::xs => {
        // remove the old transition
        val x1 = x._1
        val x2 = x._2

        val combined = s"${x1},${x2.substring(1)}"
        for (x <- alphabet) {
           
            val new1 = transitionMap((x1, x))
            val new2 = transitionMap((x2, x))
            val combined1 = s"${new1},${new2.substring(1)}"
            val combined2 = s"${new2},${new1.substring(1)}"

            if (new1 == new2) {
                if (!newStates.contains(new1)) {
                    transitionMap += ((combined, x) -> combined)
                } else {
                    transitionMap += ((combined, x) -> new1)
                }
            } else {
                if (newStates.contains(combined1)) {
                    transitionMap += ((combined, x) -> combined1)
                } else {
                    transitionMap += ((combined, x) -> combined2)
                }
            }

            transitionMap -= ((x1, x))
            transitionMap -=( (x2, x))

            println(new1, new2)

        }
        generateNewTransition(xs)
    }
}
// (String, char) into a string