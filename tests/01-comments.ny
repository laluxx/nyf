; I'm an inline comment
;; I'm a line comment
;; Check out this cool [[https://github.com/laluxx/nyf][link]]
;; This is a symbol reference `x'
;; @Laluxx <- Mentions
;; The language has a rich comment system.
;;; H1
;;;; H2
;;;;; DONE H3 [1/1]
;;;;;; DONE H4 [2/3]
  ;; - [X] Checklist 1
  ;; - [X] Checklist 2
  ;; - [ ] Checklist 3
;;;;;;; H5
;;;;;;;; H6
;;;;;;;;; H7
;;;;;;;;;; H8

;;; NyDoc

;*
@fun fibonacci
@desc Calculates the nth Fibonacci number using recursion
@param n The position in the Fibonacci sequence (must be non-negative)
@return The Fibonacci number at position n
@example fibonacci(5)  ; Returns 5
@example fibonacci(10) ; Returns 55
@throws ValueError if n is negative
@author John Doe
@version 1.0.0
@since 0.1.0
@see factorial
*;
fn fibonacci(n: int): int {
  "Calculate nth Fibonacci number"

    if n < 0 {
        return -1; Error case
    }

    if n <= 1 {
        return n
    }

    return fibonacci(n - 1) + fibonacci(n - 2)
}

;*
@fun factorial
@desc Calculates the factorial of a number
@param n A non-negative integer
@return The factorial of n (n!)
@example factorial(5)  ; Returns 120
@example factorial(0)  ; Returns 1
@author Jane Smith
@version 1.2.0
*;
fn factorial(n: int): int {
  "Calculate factorial of n"

    if n <= 1 {
        return 1
    }

    return n * factorial(n - 1)
}

;*
@fun greet
@desc Generates a personalized greeting message
@param name The name of the person to greet
@param title Optional title (Mr., Mrs., Dr., etc.)
@return A formatted greeting string
@example greet("Alice")           ; Returns "Hello, Alice!"
@example greet("Smith", "Dr.")    ; Returns "Hello, Dr. Smith!"
@deprecated Use greet_v2 instead for better internationalization support
@since 0.1.0
*;

fn greet(name: str, title: str = ""): str {
    "Generate a greeting message"

    if title == "" {
        return f"Hello, {name}!"
    }

    return f"Hello, {title} {name}!"
}

;*
@fun calculate_average
@desc Computes the arithmetic mean of a list of numbers
@param numbers A list of numeric values
@return The average value, or 0 if the list is empty
@throws DivisionByZero if the list is empty (in strict mode)
@example calculate_average([1, 2, 3, 4, 5])  ; Returns 3.0
@example calculate_average([])               ; Returns 0.0
@todo Add support for weighted averages
@todo Implement median and mode calculations
@see calculate_median
@author Math Team
@version 2.1.0
*;
fn calculate_average(numbers: list): float {
    "Calculate the average of a list of numbers"

    if numbers.length() == 0 {
        return 0.0
    }

    def sum = 0.0
    for num in numbers {
        sum += num
    }

    return sum / numbers.length()
}

;;; Main example demonstrating the functions

define result1 = fibonacci(10)
define result2 = factorial(5)
define result3 = greet("World")
define result4 = calculate_average([1, 2, 3, 4, 5])

;*
This is a module-level documentation block
explaining the purpose of this file.

@module examples
@desc Example file demonstrating documentation annotations
@author Documentation Team
@version 1.0.0
*;
