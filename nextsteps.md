# frontend
- rework all styles.
- make longform calculations work
  - number pad like on main calc, but extended
    - 0-9
    - +,-,*,/
    - ()
    - ^
    - left/right/bcksp/clear
  - on solve
    - parse and verify the expression is valid
      - make sure all parenthesis open and close correctly
        - entire expression within parenthesis must be valid expresion, recursively feed it into verify function
      - make sure there's no operators with no value inbetween, no handing op at the end
    - if expr fails verify
      - highlight error
    - if expr is valid
      - solve most deeply nested parenthesis
        - print new expression with simplified parenthesis
        - repeat until all parenthesis are gone
      - apply most deeply nested exponents to their bases
        - x^y^z == x^(y^z)  <-- point or order of operations
        - (x^y)^z == x^(y*z) <-- maybe more efficient?
        - print new expression without the exponent
        - repeat until all exponent are gone
      - look left to right for a multplication or division
        - if one is found
          -print new expression with that solved
          - repeat until no more multiplication or division
      - there should only be numbers and plus or minuses now
      - solve leftmost operation and print result until final result
            
# backend
- add more happy quotes
# general
- organize code
- write out way too many comments
