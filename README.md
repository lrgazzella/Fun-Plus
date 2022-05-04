# Fun-Plus

Fun-Plus is a an extention to the functional programming language introduced during the course in order to support two new data types and some related operators:
- strings
  - concatenation operator (+)
  - comparison operator (=)
  - length (#)
  - print_string
- couples
  - projection operator (.)

### Example of code

var x = ("Hello", 1) in
let _ = var y = ("b", 1) in x := y
  in print_string(x.1)
