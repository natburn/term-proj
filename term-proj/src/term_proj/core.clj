(ns push307.core
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
  "STUB"
  (update state stack conj item)
  )

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  "STUB"
  (update state stack rest)
  )

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  :STUB
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
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
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
  :STUB
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
  [state]
  (if (< (count (:integer state)) 2)
    state
    (let [arg1 (peek-stack state :integer)
               arg2 (peek-stack (pop-stack state :integer) :integer)
               popped-twice (pop-stack (pop-stack state :integer) :integer)]
               (push-to-stack popped-twice :integer (-' arg1 arg2)))))


(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  :STUB
  ;(make-push-instruction state -' [:integer :integer] :integer)
  (integer_-_without_helpers state))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  :STUB
  (make-push-instruction state *' [:integer :integer] :integer)
  )


(defn integer_%_without_helpers
  [state]
  (if (< (count (:integer state)) 2)
    state
    (let [arg1 (peek-stack state :integer)
               arg2 (peek-stack (pop-stack state :integer) :integer)
               popped-twice (pop-stack (pop-stack state :integer) :integer)]
               (if (= arg2 0)
                arg1
                (push-to-stack popped-twice :integer (quot arg1 arg2))))))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  :STUB
  (integer_%_without_helpers state)
  )

;;;;;;;;;;
;; Interpreter

(defn instruction?
  [ins arg]
      (loop [instructions ins]
        (if (empty? instructions)
          false
          (if (= (first instructions) arg)
            true
            (recur (rest instructions))))))


(defn push-literal-to-stack
  [new-push-state top]
  (cond 
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
  ; check if instruction or literal
  ; if instruction, make-push-instruction
  ; if literal, push to appropriate stack
  (let [top (peek-stack push-state :exec)
       new-push-state (pop-stack push-state :exec)] 
    (if (instruction? instructions top)
      ((eval top) new-push-state)
      (push-literal-to-stack new-push-state top)
  )))


(defn load-program
  [program start-state]
  (update start-state :exec concat program))



(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  ":STUB"
  (let [loaded-state (load-program program start-state)]
  (loop [current-state loaded-state]
    (println (get current-state :exec))
    (println (get current-state :exec) (get current-state :integer) (get current-state :string))
    (if (empty? (get current-state :exec))
      current-state
      (let [new-state (interpret-one-step current-state)]
        (recur new-state))))))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  :STUB
   (take max-initial-program-size (repeatedly #(nth instructions (int (rand (count instructions))))))))


(defn get-random-sample
  [population]
  (let [sample (random-sample 0.5 population)]
    (if (empty? sample)
      (get-random-sample population)
      sample)))


(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size.";"ASK PROF for this one";( let [fraction-of-population (get-random-sample population)]
  [population]
  :STUB
  (if (empty? population)
    (population)
      (loop [pop (get-random-sample population)
           fittest-ind (first pop)
           fittest-error (get (first pop) :total-error)]
      (if (= 1 (count pop))
        fittest-ind
        (if (< fittest-error (get (second pop) :total-error))
          (recur (rest pop) fittest-ind fittest-error)
          (recur (rest pop) (second pop) (get (second pop) :total-error)))))))

(defn get-element
  "return one element based on 50% chance for each"
  [element1 element2]
  (def x (+ (rand-int 10) 1))
  (if (< x 6)
    (if (nil? element1) 
      element2
      element1)
    (if (nil? element2) 
      element1
      element2)))

(defn Keep-Half?
  "return true or false based on a 50% chance"
  [n]
  (def x (+ (rand-int 100) 1))
  (if (< x 50)
    false
    true))



(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [program1 program2]
  (def element (get-element (first program1) (first program2)))
  (cond 
    (and (empty? program1) (empty? program2)) 
            '()
    (and (empty? (rest program1)) (not (empty? (rest program2)))) 
            (cons element (filter Keep-Half? (rest program2)))

    (and (empty? (rest program2)) (not (empty? (rest program1)))) 
            (cons element (filter Keep-Half? (rest program1)))
    :else 
      (cons element (crossover (rest program1) (rest program2)))))


(defn add-instructions
  ;returns the list with randomly selected instructions inserted
  ;as indicated in the index_insert list
  [index_insert program instructions pos]
  (def current_index (first index_insert))
  (def current_program_value (first program))
  (def instruction (rand-nth instructions))
  (if (and (nil? current_program_value) (nil? current_index))
    '()
   (if (= current_index pos)
    (cons instruction 
                            (add-instructions (rest index_insert) 
                                              program 
                                              instructions
                                              pos))
    (cons current_program_value
                            (add-instructions index_insert 
                                              (rest program) 
                                              instructions
                                              (inc pos))))))
(defn Insert?
  ;returns true to give a 5% probability if rand num is less than 5
  [n]
  (def x (+ (rand-int 100) 1))
  (if (< x 6)
    true
    false))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [program]
  (def x (range (inc (count program))))
  ;indices of where to insert instructions
  (def index_insert (filter Insert? x))
  (add-instructions index_insert program instructions 0))

(defn keep?
  "return true if x is 5 or less"
  [n]
  (def x (+ (rand-int 100) 1))
  (if (< x 6)
  false
  true))

(defn uniform-deletion
  "Randomly deletes instructions from program at some rate. Returns child program."
  [prog]
  (filter keep? prog))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population]
  :STUB
  (let [chance (int (rand 4))
        program1 (get (tournament-selection population) :program)
        program2 (get (tournament-selection population) :program)
        child-program (cond
                          (< chance 2) (crossover program)
                          (= chance 2) (uniform-addition program) 
                          (= chance 3) (uniform-deletion program) )]
        {:program child-program}))
;2001:0db8:0000:0000:000:ff00:0042:8329
(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  :STUB
  (println "-------------------------------------------------------")
  (println "               Report for Generation ", generation)
  (println "-------------------------------------------------------")
  (let [best_individual (get-best-individual population)])
  )

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
  :STUB
  )


;;;;;;;;;;
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  :STUB
  (+ (+ x 3) (* (* x x) x)))


;(def example-individual
; {:program '(3 5 integer_* "hello" 4 "world" integer_-)
; :errors [8 7 6 5 4 3 2 1 0 1]
; :total-error 37})"



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
  :STUB
  (def test-cases (range 0 5 101))
  (def program (get individual :program))
  (loop [test-case (first test-cases)
        errors []]
    (let [begin-state ((assoc-in empty-push-state [:input] {:in1 5}))
         result-state (interpret-push-program (program begin-state))
         result (get :integer result-state)
         target-result (target-function test-case)]
         (if (not (= result  target-result))
              (recur (rest test-cases) '(conj (abs (- result target-result)) errors))))))




(defn get-test-case
  [test-case]
  (assoc-in empty-push-state [:input] {:in1 test-case}))

(defn get-result-state
  [test-case program]
  (def start-state (assoc-in empty-push-state [:input] {:in1 test-case}))
  (def result-state (interpret-push-program program start-state))
  (def result (get result-state :integer))
  (if (empty? result)
    1000
    (first result)
    ))



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
  :STUB
  (loop [test-cases (range 0 101 5)
         program (get individual :program)
         errors '()]
         ;(println "here", (first test-cases))
         (println "here", (first test-cases),"result" (get-result-state (first test-cases) program))
         (if (empty? test-cases)
          errors
         (recur (rest test-cases)
                 program
                 (conj errors
                      (get-result-state (first test-cases) program))))))
;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]"
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 500
            :population-size 200
            :max-initial-program-size 50}))"
  (interpret-push-program example-push-program))

