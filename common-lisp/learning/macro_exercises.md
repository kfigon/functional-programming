# 20 Common Lisp Macro Exercises

**Difficulty:** Beginner → Intermediate → Advanced  
**Language:** Common Lisp  
**Goal:** Master macro writing, evaluation control, code generation, variable capture, and DSL design.

## Level 1 — Beginner: Basic Macros

### 1. Unless Macro
Implement `my-unless`, which evaluates its body only when the condition is false.

Example:
```lisp
(my-unless (> 3 5)
  (format t "Condition is false!"))
```
Requirements:
- Accept one condition and multiple body forms.
- Return the last evaluated body's result.
- Return `NIL` when the condition is true.

### 2. When Macro
Implement `my-when`, which executes its body only when the condition is true.

Example:
```lisp
(my-when (> 10 5)
  (format t "First statement~%")
  (+ 10 20))
```
Requirements:
- Accept multiple body forms.
- Return the last body's result.
- Return `NIL` when the condition is false.

### 3. Infix Addition
Create an `infix+` macro accepting two expressions and expanding into addition.

Example:
```lisp
(infix+ 10 20)
;; => 30
```
Requirements:
- Accept exactly two arguments.
- Expand into a `+` expression.
- Preserve normal evaluation semantics.

### 4. Debug Macro
Implement `debug-print`, which prints an expression and its evaluated result.

Example:
```lisp
(debug-print (+ 10 20))
;; (+ 10 20) => 30
```
Requirements:
- Print the original expression.
- Evaluate the expression exactly once.
- Return its evaluated result.

### 5. Timing Macro
Implement `time-execution`, which measures how long its body takes to execute.

Example:
```lisp
(time-execution
  (loop repeat 1000000 sum 10))
```
Requirements:
- Use `get-internal-real-time`.
- Execute the body exactly once.
- Print elapsed time in seconds.
- Return the body's result.

## Level 2 — Intermediate: Evaluation and Control Flow

### 6. Repeat Macro
Create `repeat-times`, which evaluates a body a specified number of times.

Example:
```lisp
(repeat-times 3
  (format t "Hello!~%"))
```
Requirements:
- Accept a count expression.
- Evaluate the count only once.
- Support multiple body forms.
- Return `NIL` after execution.

### 7. Conditional Assignment
Implement `set-if`, which assigns a value to a place only if a condition is true.

Example:
```lisp
(let ((x 10))
  (set-if (> x 5) x 100)
  x)
;; => 100
```
Requirements:
- Accept a condition, generalized place, and value.
- Do not evaluate the value if the condition is false.
- Use `setf`.

### 8. Swap Macro
Implement `swap` to exchange the values of two generalized places.

Example:
```lisp
(let ((a 10)
      (b 20))
  (swap a b)
  (list a b))
;; => (20 10)
```
Requirements:
- Support generalized places.
- Evaluate place subforms correctly.
- Avoid temporary variable name collisions using `gensym`.

### 9. With-Open-File Wrapper
Create `with-file`, a macro that wraps file operations.

Example:
```lisp
(with-file (stream "example.txt")
  (format stream "Hello, Common Lisp!"))
```
Requirements:
- Open a file for output.
- Close the stream automatically.
- Use `with-open-file` internally.
- Allow multiple body forms.

### 10. Assert Macro
Implement `my-assert`.

Example:
```lisp
(my-assert (> 10 5))
```
Requirements:
- Evaluate the condition only once.
- Signal an error when the condition is false.
- Include the failed expression in the error message.
- Return `T` when successful.

## Level 3 — Intermediate/Advanced: Macro Techniques

### 11. In-Range Macro
Implement `in-range`, which checks whether a number falls between two boundaries.

Example:
```lisp
(in-range 5 1 10)
;; => T
```
Requirements:
- Accept a value, lower bound, and upper bound.
- Evaluate each argument exactly once.
- Use `gensym` to prevent variable capture.
- Support inclusive boundaries.

### 12. Anaphoric If
Implement `aif`, an anaphoric conditional macro.

Example:
```lisp
(aif (find 3 '(1 2 3 4))
     (format t "Found: ~a~%" it)
     (format t "Not found~%"))
```
Requirements:
- Bind the result of the condition to `it`.
- Make `it` available in both branches.
- Evaluate the condition only once.

### 13. With-Temporary-Variable
Create `with-temp`, which introduces a temporary lexical variable.

Example:
```lisp
(with-temp (x (+ 10 20))
  (* x 2))
;; => 60
```
Requirements:
- Create a lexical binding.
- Restrict visibility to the body.
- Support multiple body forms.
- Consider how to avoid accidental variable capture.

### 14. In-Place Modification
Implement `toggle`, which switches a generalized place between `T` and `NIL`.

Example:
```lisp
(let ((enabled t))
  (toggle enabled)
  enabled)
;; => NIL
```
Requirements:
- Accept a generalized place.
- Use `setf`.
- Return the new value.
- Ensure place subforms are not evaluated unnecessarily.

### 15. Multiple Value Binding
Implement `multiple-value-bind*`, which supports sequential multiple-value bindings.

Example:
```lisp
(multiple-value-bind*
    ((a b) (values 10 20))
    ((c) (+ a b))
  (list a b c))
;; => (10 20 30)
```
Requirements:
- Bind multiple values from each expression.
- Make previous bindings available to subsequent expressions.
- Support any number of binding groups.
- Handle zero returned values correctly.

## Level 4 — Advanced: Practical Macro Design

### 16. Pipeline Macro
Create `->`, which threads a value through a sequence of functions.

Example:
```lisp
(-> 5
    (1+)
    (* 2)
    (- 3))
;; => 9
```
Requirements:
- Pass the previous result as the first argument of the next form.
- Support multiple transformation steps.
- Handle a single initial expression.
- Preserve normal evaluation semantics.

### 17. Short-Circuit OR
Implement `my-or`, which evaluates expressions from left to right and stops when one returns a non-`NIL` value.

Example:
```lisp
(my-or
  nil
  nil
  (+ 10 20)
  (error "Should never execute"))
;; => 30
```
Requirements:
- Evaluate each expression at most once.
- Stop at the first non-`NIL` result.
- Return the actual result rather than just `T`.
- Handle zero arguments.

### 18. Resource Management Macro
Implement `with-resource`, which guarantees cleanup.

Example:
```lisp
(with-resource
    (resource (acquire-resource))
    (release-resource resource))
  (use-resource resource))
```
Requirements:
- Acquire a resource before executing the body.
- Execute cleanup even if the body signals an error.
- Use `unwind-protect`.
- Ensure cleanup is executed exactly once after successful acquisition.

### 19. Compile-Time Validation
Create `define-constant*`, which defines a constant while validating its name and value at macro-expansion time.

Example:
```lisp
(define-constant* +max-users+ 100)
```
Requirements:
- Check that the name is a symbol.
- Check that the name follows the `+NAME+` naming convention.
- Reject invalid definitions during macro expansion.
- Expand into a normal `defconstant` form.

Hints: Explore `eval-when`, `error`, and `symbol-name`.

### 20. Mini DSL — State Machine
Create `define-state-machine`, which allows users to define a state machine using declarative syntax.

Example:
```lisp
(define-state-machine traffic-light
  (:initial red)
  (:states
    red
    yellow
    green)
  (:transitions
    (red -> green)
    (green -> yellow)
    (yellow -> red)))
```
Requirements:
- Define a state machine with a name.
- Accept an initial state.
- Accept a list of valid states.
- Accept a list of transitions.
- Generate a function called `traffic-light-next`.
- Return the next state.
- Signal an error when an invalid state is provided.
- Validate transitions during macro expansion.

Additional challenges:
- Generate a `traffic-light-state-p` predicate.
- Generate a `traffic-light-reset` function.
- Reject transitions referencing undefined states.

## Bonus Challenges

1. Implement `while` using recursion or generated looping constructs.
2. Implement `for` with Python-like iteration syntax.
3. Implement `once-only`, a macro-writing utility that evaluates specified forms exactly once.
4. Implement `condp`, a pattern-based conditional macro.
5. Implement `with-gensyms` to simplify macro construction.
6. Implement a simplified `match` macro supporting literals, variables, and wildcards.
7. Implement a `do-transaction` macro with rollback behavior.
8. Implement a macro that generates accessor functions for a data structure.

## Recommended Common Lisp Concepts

| Concept | Purpose |
|---|---|
| `defmacro` | Defining macros |
| `macroexpand-1` | Inspecting one expansion step |
| `macroexpand` | Inspecting complete expansion |
| `gensym` | Preventing accidental variable capture |
| `&body` | Accepting macro bodies |
| `&environment` | Accessing macro-expansion environments |
| `&whole` | Capturing the complete macro form |
| `setf` | Working with generalized places |
| `unwind-protect` | Guaranteed cleanup |
| `eval-when` | Controlling compile/load/execute behavior |
| `multiple-value-bind` | Handling multiple return values |
| `symbol-macrolet` | Symbol macro bindings |
| `macrolet` | Defining local macros |
| `compiler-macro` | Optimizing function calls |

## Self-Assessment Checklist

- [ ] I can write basic macros using `defmacro`.
- [ ] I understand the difference between macros and functions.
- [ ] I understand when expressions are evaluated.
- [ ] I can prevent multiple evaluation of arguments.
- [ ] I understand variable capture.
- [ ] I can use `gensym` effectively.
- [ ] I can work with generalized places and `setf`.
- [ ] I can generate complex Lisp forms.
- [ ] I understand macro-expansion-time validation.
- [ ] I can design a small domain-specific language (DSL).

## Final Goal

By completing all 20 exercises, you should be comfortable writing Common Lisp macros that go beyond saving typing: designing new abstractions, controlling evaluation, generating code, and extending the language with domain-specific syntax.