# frontend
- make longform calculations work
  - segment datatype
  ```
  data Segment = Operator Text
               | Number Text
               | NumWCur (Text, Text)
               | Paren Bool -- True is open, False is closed
               | Curs
               | Var -- maybe add later
      Deriving Show 
      ```
  - solver model datype
  ```
  data Equation = 
    Equation {
      _equation_segments :: [Segment]
    , _equation_solvable :: Bool 
    }
  ```
  - events to update Equation state
    - updated numPad
      - if there is a NumWCur in [Segment]
        - do nothing, numpad internal state will deal
      - else, add a new NumWCur in [Segment] where Curs is
    - opPad (Text)
      - if there is a Curs in [Segment]
        - place Operator Text infront of Curs in [Segment]
      - else replace NumWCur with two Num Text in [Segment], place Operator Text in between
    - paren
      - if there is a curs
        - add paren right before it
      - else
        - split NumWCur into two Num, with paren in between
    - bcksp
      - if there is a Curs and it's not the first element
        - remove the Segment before Curs
      - else do nothing
    - curs left
      - if there is a Curs and it's not the first Segment
        - swap curs with [Segment] to the left
    - curs right
      - if there is a Curs and it's not the last Segment
        - swap curs with [Segment] to the right
    - clear
      - replace [Segment] with Curs:[]
  - 3 input pads
    - number pad widget
      - on first click start NumWCur segment of equation
      - when click away, convert -. to -0., preface leading . with 0
      - if trailing or naked . or naked -, then highlight problem instead of leaving segment
      - after leaving segment, push it in the correct spot in the list of equation segments
      - if click away to an open parenthesis, add a multiplication sign inbetween, making implicit explicit
      - if click on op while curser is in number, split num and put op in between with cursor after
    - operators/symbols widget
      - opPad :: AppWidget js t m => m(Event t Text)
        - +, -, /, *, ^
      - on fire push operator before cursor in [Segment] 
    - parenthesis widget
      - parenPad :: AppWidget js t m => m(Dynamic t Int)
      - keep track of number of parenthesis level
        - on ( add Paren [Curs] to [Segment], inc paren level count
        - if curs is in a Paren, then all buttons push to that [Segment]
        - close paren only available if paren level > 0
          - on close paren, move cursor and everthing after from Paren [Segment] to outer [Segment]
    - on every _solver _segments change
      - solver_solvable is False if
        - paren level is > 0
        - main or any nested [Segment] either
          - ends with operator
          - contains 2 consecutive operators or numbers
  - show [Segment]
  - on solve
    - if solver_solvable == False
      - if parenLevel > 0 print that there are open parens still
      - else move cursor to first problem
        - print weather its two consecutive nums, ops, or just a trailing op
    - else if length [Segment] == 1
      - render Segment
    - else if there are parenthesis
      - recurse on [Segment] on parenthesis
      - replace paren in [Segment] with Number final answer
      - show new [Segment]
      - if length new [Segment] == 1 return ()
      - else recurse on solve with new [Segment]
    - else if there are Exponent op
        - x^y^z == x^(y^z)  <-- point or order of operations
        - (x^y)^z == x^(y*z) <-- maybe more efficient?
      - find consecutive Exp Ops with only one number between
      - replace last n^y of each streak with ans
      - recurse with new [Segment]
    - else if there are x or / ops
      - find consecutive mul or div ops
        - replace first x*y or x/y of each streak with answer
        - recurse with new [Segment]
    - else only nums and + - ops should remain
      - replace first x+y or x-y with answer
      - recurse with new [Segment]
- graphing calculator
  - Dynamic t (Int, Int) to represent initial graph size and dimensions
  - Dynamic t (Int, Int) how far off center the view of the graph is
  - num, operator, and variable input
    - only variable are x and y
  - on print, map functions [range of visible x]
  - zip both into [(x, y)] coords
  - mark appropriate grid on graph
  - on graph resize or scroll, adjust numbers for size and view, recalculate, and rerender
- make it look good
            
# backend
- add more happy quotes
# general
- write out way too many comments
