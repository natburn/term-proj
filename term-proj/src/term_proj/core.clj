;;;; Muhammad Najib & Nathaniel Colburn
;;;; Professor Helmuth
;;;; CS 307
;;;; 9 October 2017
;;;; Genetic Programming Term Project Basic Implementations
;;;; The program creates a random population of given instructions
;;;; and evolves a program that attemps to match the target function
;;;; in its output. Program is evolved using tournament selection and
;;;; crossover, mutation and addition genetic operators.

(ns term-proj.core
  (:gen-class))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Push program
(def example-push-program
  '(in1 integer_* "hello" 4 "world" integer_-))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error 
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  '( 
    in1
   integer_+
   integer_-
   integer_*
   integer_%
   0
   1
   ))

(def non-literal-instructions
  '( 
    in1
   integer_+
   integer_-
   integer_*
   integer_%
   ))

;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (update state stack conj item)
  )

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (update state stack rest)
  )

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))
  ))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (get state stack))
  )

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks) 
         args '()]
    ;if stacks is empty, return state and args map
    (if (empty? stacks)
      {:state state :args args}
      ;if stack empty, return :not enough args
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          ;else make recursive call and concatenate
          ;stack top to args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))


;;;;;;;;;;
;; Instructions
(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (let [result (get (get state :input) :in1)]
    (push-to-stack state :exec result)
  ))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

;;;; This is an example of what would be necessary to implement integer_+
;;;; without the useful helper function make-push-instruction.
;; (defn integer_+_without_helpers
;;   [state]
;;   (if (< (count (:integer state)) 2)
;;     state
;;     (let [arg1 (peek-stack state :integer)
;;           arg2 (peek-stack (pop-stack state :integer) :integer)
;;           popped-twice (pop-stack (pop-stack state :integer) :integer)]
;;       (push-to-stack popped-twice :integer (+' arg1 arg2)))))

(defn integer_-_without_helpers
  "the meat of integer_-
  Subtracts the top integer from the second integer and leaves the
  result on the integer stack. If the integer stack has fewer than
  two elements, returns the state."
  [state]
  (if (< (count (:integer state)) 2) ; ensure two args on int stack
    state     
    (let [arg1 (peek-stack state :integer) ; get top
          arg2 (peek-stack (pop-stack state :integer) :integer) ; get second
          ; create stack with both popped off
          popped-twice (pop-stack (pop-stack state :integer) :integer)]
      ; push difference to the popped stack
      (push-to-stack popped-twice :integer (-' arg1 arg2))))) ; 

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  (integer_-_without_helpers state))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%_without_helpers
  "the meat of integer_%
  Divides the second integer by the top integer and leaves the
  result on the integer stack. If the integer stack has fewer than
  two elements, returns the state."
  [state]
  (if (< (count (:integer state)) 2)
    state 
    (let [arg1 (peek-stack state :integer)
          arg2 (peek-stack (pop-stack state :integer) :integer) 
          popped-twice (pop-stack (pop-stack state :integer) :integer)]
      (if (= arg2 0) ; protected division
        arg1 ; return numerator
        (push-to-stack popped-twice :integer (quot arg1 arg2))))))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (integer_%_without_helpers state))

;;;;;;;;;;
;; Interpreter

(defn instruction?
  "returns true if given list contains given element"
  [ins arg]
      (loop [instructions ins]
        (if (empty? instructions) ; reached end of list 
          false
          (if (= (first instructions) arg) ; found!
            true
            (recur (rest instructions)))))) 

(defn push-literal-to-stack
  "Pushes given element 'top' to the correct stack based on comparison
  using the type function. Only does so if int, string, or input, other-
  wise returns given push state unchanged."
  [new-push-state top]
  (cond 
    ;if integer, string or input push to its stack else return original state
    (= (type top) (type 1))  (push-to-stack new-push-state :integer top) 
    (= (type top) (type "abc"))  (push-to-stack new-push-state :string top)
    (= (type top) (type {:in1 1}))  (push-to-stack new-push-state :input top)
    :else new-push-state))

;;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (let [top (peek-stack push-state :exec)
       new-push-state (pop-stack push-state :exec)] 
    (if (instruction? non-literal-instructions top) ; check instruct or literal
      ((eval top) new-push-state) ; execute instruction
      (push-literal-to-stack new-push-state top))))

(defn load-program
  "adds a program to the exec stack"
  [program start-state]
  (update start-state :exec concat program))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  (let [loaded-state (load-program program start-state)]
  (loop [current-state loaded-state]
    (if (empty? (get current-state :exec)) 
      current-state ; state when exec is empty
      (let [new-state (interpret-one-step current-state)]
        (recur new-state))))))

;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-len]
  {:program (take max-len (repeatedly #(nth instructions (int (rand (count instructions))))))})

(defn get-random-sample
  "creates a population sample, including each element of population
  with a 50% chance"
  [population]
  (let [sample (random-sample 0.5 population)]  
    (if (empty? sample) ;just in case none are selected
      (get-random-sample population)
      sample)))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (if (empty? population) 
    population
    (loop [pop (get-random-sample population) ; randomly selects population
           fittest-ind (first pop) ; saves first to be compared
           fittest-error (get (first pop) :total-error)]
      (if (= 1 (count pop)) ; when at end of pop, return latest individual
        fittest-ind
        ; keep individual with lower error
        (if (< fittest-error (get (second pop) :total-error)) 
          (recur (rest pop) 
                 fittest-ind ; fittest unchanged
                 fittest-error) 
          (recur (rest pop)
                 (second pop) ; fittest changes
                 (get (second pop) :total-error))))))) 

(defn get-element
  "return one element based on 50% chance for each"
  [element1 element2]
  (let [x (+ (rand-int 10) 1)]
  (if (< x 6)
    (if (nil? element1) 
      element2
      element1)
    (if (nil? element2) 
      element1
      element2))))

(defn keep-half?
  "return true or false based on a 50% chance"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 50)
    false
    true)))

(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [program1 program2]
  ;get the element randomly from one of the programs
  (let [element (get-element (first program1) (first program2))]
  (cond 
    ;if both progs empty return null
    (and (empty? program1) (empty? program2)) 
            '()
    ;filter the other list if one of them is empty
    (and (empty? (rest program1)) (not (empty? (rest program2)))) 
            (cons element (filter keep-half? (rest program2)))
    (and (empty? (rest program2)) (not (empty? (rest program1)))) 
            (cons element (filter keep-half? (rest program1)))
    ;recursive call to perform crossover for next elements
    :else 
        (cons element (crossover (rest program1) (rest program2))))))

(defn add-instructions
  "returns the list with randomly selected instructions inserted
   as indicated in the index_insert list"
  [index_insert program instructions pos]
  (let [current_index (first index_insert)
        current_program_value (first program)
        instruction (rand-nth instructions)]
        ;return nil program and index are nil
        (if (and (nil? current_program_value) (nil? current_index))
           '()
           ;if index matches for position pos add instruction
           ;at this index without incrementing pos so original
           ;instruction can be added on next loop
           (if (= current_index pos)
            (cons instruction 
                  (add-instructions (rest index_insert) 
                                     program 
                                     instructions
                                     pos))
            ;if current_index not equal to pos
            ;go to next index with rest of program 
            ;next position
            (cons current_program_value
                  (add-instructions index_insert 
                                    (rest program) 
                                    instructions
                                    (inc pos)))))))

(defn insert?
  "returns true to give a 5% probability if rand num is less than 5"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 6)
    true
    false)))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [program]
  (let [x (range (inc (count program)))
        ;indices of where to insert instructions
        index_insert (filter insert? x)]
        (add-instructions index_insert program instructions 0)))

(defn keep?
  "return true if x is 5 or less"
  [n]
  (let [x (+ (rand-int 100) 1)]
  (if (< x 6)
    false
    true)))

(defn uniform-deletion
  "Randomly deletes instructions from program at some rate. Returns child program."
  [prog]
  (filter keep? prog))

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ (+ x 3) (* (* x x) x)))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population]
  (let [chance (int (rand 4))
        program1 (get (tournament-selection population) :program)
        program2 (get (tournament-selection population) :program)
        ;pick one of the genetic operators randomly
        child-program (cond
                          (< chance 2) (crossover program1 program2)
                          (= chance 2) (uniform-addition program1) 
                          (= chance 3) (uniform-deletion program1))]
                      ;return program returned by operator
                      {:program child-program}))

(defn get-best-individual
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (if (empty? population)
    population
    (loop [population population
           fittest-ind (first population)
           fittest-error (get (first population) :total-error)]
          (if (= 1 (count population))
            ;if last member remaining, return the fittest
            fittest-ind
            ;compare fittest-ind with next individual and adjust fittest-ind accordingly
            (if (< fittest-error (get (second population) :total-error))
              (recur (rest population) fittest-ind fittest-error)
              (recur (rest population) (second population) (get (second population) :total-error)))))))

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).
;-------------------------------------------------------
;               Report for Generation 3
;-------------------------------------------------------
;Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
;Best program size: 33
;Best total error: 727
;Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)  "
 [population generation]
  (println "-------------------------------------------------------")
  (println "               Report for Generation ",generation)
  (println "-------------------------------------------------------")
  (let [best-individual (get-best-individual population)
        best-program (get best-individual :program)]
        (println best-program)
        (println "Best program size:" (count best-program))
        (println "Best total error:", (get best-individual :total-error))
        (println "Best errors:", (get best-individual :errors))))



(defn get-initial-population
  "initialize the population randomly"
  [population-size max-initial-program-size]
  (take population-size (repeatedly #(make-random-push-program instructions max-initial-program-size))))

(defn is-solution?
  "checks if the total error for the ind in 0"
  [individual]
  (let [total-error (get individual :total-error)]
    (if (= 0 total-error)
      true
      false)))


(defn find-solution
  "checks if the solution has been found in the current generation"
  [population]
  (loop [population population]
        (if (empty? population)
          false
          (if (is-solution? (first population))
            true
            (recur (rest population))))))

(defn get-test-case
  "returns the state with test case pushed to the :input"
  [test-case]
  (assoc-in empty-push-state [:input] {:in1 test-case}))

(defn get-test-result
  "returns the error for a given test case evaluated on a program"
  [test-case program]
  ;start-state - state with in1 having input
  ;result-state - result state with integer stack containing result
  ;result - result as taken from the integer stack
  (let [start-state (assoc-in empty-push-state [:input] {:in1 test-case})
        result-state (interpret-push-program program start-state)
        result (get result-state :integer)]
        ;if integer stack is empty return a large error, 
        ;else return the difference
        (if (empty? result)
          100000
          (Math/abs (- (first result) (target-function test-case))))))

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (loop [test-cases (range -100 101 7)
         program (get individual :program)
         errors '()]
         ;if all test cases applied return individual with errors
         ;and :total error included
         (if (or (empty? test-cases) (nil? test-cases))
            (-> individual
                (assoc-in  [:errors] errors)
                (assoc-in [:total-error] (reduce +' errors)))
            (recur (rest test-cases)
                    program
                    (conj errors
                          (get-test-result (first test-cases) program))))))


(defn create-new-generation
  "repreatedly creates a new generation for the given pop size using select-and-vary."
  [population-with-errors population-size]
  (take population-size (repeatedly #(select-and-vary population-with-errors))))

;;;;;;;;;;
;; The main gp algorithm. 
(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  ;population - this parameter contains initialized population and those returned by tournament selection
  ;population-with-errors - evaluates errors and total-error for each individual of population
  (loop [population (get-initial-population population-size max-initial-program-size)
         population-with-errors (map regression-error-function population)
         generation 0]
        (report population-with-errors generation)
        (cond 
          ;return if end-state reached
          (= generation max-generations) nil
          (find-solution population-with-errors) :SUCCESS
          :else
              ;if end-state not reached, evolve a new generation and make the recursive call
              (let [next-generation (create-new-generation population-with-errors population-size)]
                    (recur next-generation (map regression-error-function next-generation) (inc generation)))))) 
;;;;;;;;;;
;; The main function. Uses some problem-specific functions.
(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 500  
            :population-size 200
            :max-initial-program-size 50}))
