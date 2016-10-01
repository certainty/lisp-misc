;;; first.scm --- 
;; 
;; Filename: first.scm
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: Di Jun 24 20:40:49 2008 (CEST)
;; Version: 
;; Last-Updated: So Aug 10 22:18:54 2008 (CEST)
;;           By: closure
;;     Update #: 402
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; This is an implementation of a genetic algorithm (GA) to solve the n-queens-problem.
;; The code is based on the ideas presented in AIMA (Artificial Intelligence A Modern Approach).
;; Especially the fitnessfunction proposed there is used here.
;; Terminology and general algorithm:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(use (srfi 1 13))

;;some srfi parameters that control the algorithm
;;the mutation-rate must be a value between 0.0 and 1.0
(define mutation-rate (make-parameter .2))

;;crossover-rate determines how often a crossover happens
;;value must be between 0.0 and 1.0
(define crossover-rate (make-parameter .7))

;;the size of the population
;;value must be even
(define population-size (make-parameter 20))

;;the problem size
(define number-of-queens (make-parameter 4))

;;print debug messages 
(define do-debug (make-parameter #f))



;;The main algorithm is as follows
;;1) create an initial population with random chromosomes
;;2) check if there are solutions, if yes exit and present them
;;3) select chromosomes for reproduction based on the fitness of each
;;4) crossover chromosomes, mutate if needed
;;5) replace the chromosomes in the population with the offspring
;;6) goto step 2
(define (solve-n-queens)
  (ensure-sane-parameters)
  (let next-round ((population (initial-population)) (generation 1))
    (debug "Generation: ~A~%" generation)
    (debug "Population: ~A~%" population)
    (let ((solutions (find-solutions population)))
      (if (null? solutions)
          (next-round (offspring population) (add1 generation))
          (present-solutions generation solutions)))))

(define (debug fmt . args)
  (when (do-debug) (apply printf fmt args)))

;;ensure that the parameters are valid
(define (ensure-sane-parameters)
  (when (< (population-size) 4)
    (error "Population-size too small to find solutions"))
  (when (or (> (mutation-rate) 1.0) (< (mutation-rate) 0.0))
    (error "Invalid value for mutation-rate (0.0-1.0)"))
  (when (or (> (crossover-rate) 1.0) (< (crossover-rate) 0.0))
    (error "Invalid value for crossover-rate (0.0-1.0)"))
  (unless (zero? (modulo (population-size) 2))
    (error "Invald value for population-size (must be even)")))

;;A population is a list of chromosomes
;;A chromosome if a list where each value is a fixnum between 1 and number-of-queens
;;The length of each chromosome if number-of-queens.
;;The index of a particular element serves as the column-value wheras the actual fixnum serves as the row-value
(define (initial-population)
  (let ((random-value (lambda (_) (random/1-noq))))
    (randomize)
    (list-tabulate (population-size)  (lambda (_) (list-tabulate (number-of-queens) random-value)))))

;;helper to produce a random fixnum between 1 and number-of-queens
(define (random/1-noq)
  (let ((val (random (+ 1 (number-of-queens)))))
    (if (zero? val) 1 val)))

;;filter solutions
;;a solution is every chromosome that has optimal fitness
(define (find-solutions population)
  (filter (lambda (chromosome) (= (optimal-fitness) (fitness chromosome))) population))

;;optimal fitness is the maximum number of non-attacking pairs
;;this value depends on the number-of-queens and is directly derived
;;from the ordinary arithmetic series sum(n,k=1) = 1 + 2 + 3 ... n - 1 = n * (n - 1) / 2
(define (optimal-fitness)
   (/ (* (number-of-queens) (- (number-of-queens) 1)) 2))

;;the fitness of a chromosome is measured in the number of non-attacking pairs
(define (fitness chromosome)
  (pair-of-queens-fold (lambda (q1 q2 accu) (+ accu (if (attacking? q1 q2) 0 1))) 0 chromosome))

;;helper to fold over the list additionally passing the index of the current element
(define (fold/index kons index knil ls)
    (if (null? ls) knil (fold/index kons (+ index 1) (kons index (car ls) knil) (cdr ls))))

;;helper to fold over a pair of queens in a given chromosome
(define (pair-of-queens-fold kons knil ls) 
  (define (poq-fold i kons knil ls)
    (if (null? (cdr ls)) knil
        (let ((i-elt (car ls))
              (rest (cdr ls)))
          (poq-fold (+ i 1) kons (fold/index (lambda (j j-elt accu) (kons (cons i i-elt) (cons j j-elt) accu))
                                             (+ i 1)
                                             knil
                                             rest) rest))))
  
  (if (null? ls) knil (poq-fold 1 kons knil ls)))

;;two queens are attacking each other if they are placed on the same row
;;or lay on the same diagonals
(define (attacking? queen1 queen2)
  (or (= (row queen1) (row queen2))
      (same-diagonal? queen1 queen2)))

(define col car)

(define row cdr)

;;this is simple linear algebra
;;see simple straight-functions y = mx + b
(define (same-left-to-right-diagonal? queen1 queen2)
  (let ((b (- (row queen1) (col queen1))))
    (= (row queen2) (+ (col queen2) b))))

;;the is the crossing diagonal (a projection of the above one)
;;we get it by mirroring the coordinates-system at the right end
(define (same-right-to-left-diagonal? queen1 queen2)
  (let* ((x1 (+ 1 (- (number-of-queens) (col queen1))))
         (x2 (+ 1 (- (number-of-queens) (col queen2))))
         (b (- (row queen2) x2)))
    (= (row queen1) (+ x1 b))))

(define (same-diagonal? queen1 queen2)
  (or (same-left-to-right-diagonal? queen1 queen2)
      (same-right-to-left-diagonal? queen1 queen2)))

;;generate a new population
(define (offspring population)
  (define (reproduce parent1 parent2 ls)
    (receive (child1 child2) (crossover parent1 parent2)
      (cons (mutate child1) (cons (mutate child2) ls))))
  (selection-fold reproduce '() population))

;;for the crossover we select a random crossover-point (an index in the list)
;;we chop of the head at this point on each parent and swap tails.
;;Then we merge the head of the first parent with the tail of the second and vice versa
(define (crossover parent1 parent2)
  (if (< (random-flonum) (crossover-rate))
      (values parent1 parent2)
      (let ((crossover-point (random (number-of-queens))))
        (let-values (((head1 tail1) (split-at parent1 crossover-point))
                     ((head2 tail2) (split-at parent2 crossover-point)))
          (values (append head1 tail2) (append head2 tail1))))))

  

;;generate a random flonum 0 < f < 1
(define (random-flonum #!optional (rand-max 1000000))
  (/ (random rand-max) (/ (+ rand-max 1) 1)))  


;;less aggressive version of mutate
;;it exits the process as soon as one mutation accured
(define (mutate chromosome)
  (let loop ((chr chromosome) (accu '()))
    (cond
     ((null? chr) (reverse accu))
     ((> (random-flonum) (mutation-rate))
      (append (cdr chr) (reverse (cons (random/1-noq) accu))))
     (else
      (loop (cdr chr) (cons (car chr) accu))))))                 

;;mutate at a random index by altering the value in the range 1 - number-of-queens
;(define (mutate chromosome)
;  (fold (lambda (elt ls) (cons (if (> (random-flonum) (mutation-rate)) elt (random/1-noq)) ls)) '() chromosome))

;;fold over selected chromosomes
(define (selection-fold kons knil population)
  (if (null? population) knil
      (let*-values (((c1 rest) (select-chromosome population))
                    ((c2 rest2) (select-chromosome rest)))
                   (selection-fold kons (kons c1 c2 knil) rest2))))

;;The selection-operator used here is known as fitness proportionate selection (roulette-wheel selection)
;;it is very easy and not very effective but it is simple to comprehend and thus suitable for our purposes
(define (select-chromosome population)
  (define (normalize fitness)
    (/ 1 (- (optimal-fitness) fitness)))

  (let* ((optimum (optimal-fitness))
         (total (fold (lambda (chromosome accu) (+ accu (normalize (fitness chromosome)))) 0.0 population))
         (slice (* total (random-flonum))))
    (let loop ((population population) (total 0.0) (accu '()))
      (cond
       ((null? population) (values (car (reverse population)) population))
       (else
        (let ((next-total (+ total (normalize (fitness (car population))))))
          (if (>= next-total slice)
              (values (car population) (append (reverse accu) (cdr population)))
              (loop (cdr population) next-total (cons (car population) accu)))))))))
              
(define (present-solutions generations solutions)
  (printf "Found ~A solution(s) after ~A generation(s):~%" (length solutions) generations)
  (for-each (lambda (sol) (printf "~A ~%" sol)) solutions))


;;adjust the parameters and run
;;feel free to play around with the parameters
(parameterize ((mutation-rate .3)
               (crossover-rate 1)
               (population-size 20)
               (number-of-queens 8))
  (solve-n-queens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first.scm ends here
