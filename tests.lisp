;;; tests for cl-poker-2

(in-package #:cl-poker-2)

(5am:run! 'player-tests)
(5am:run! 'bet-tests)
(5am:run! 'call-tests)
(5am:run! 'player-in-bets?-test)
(5am:run! 'handle-player-action-tests-bets)
(5am:run! 'handle-player-action-tests-raises)
(5am:run! 'handle-player-action-tests-calls)
(5am:run! 'get-acting-test)
(5am:run! 'advance-acting-test)
(5am:run! 'get-bet-test)
(5am:run! 'adjust-bets-test)
(5am:run! 'pay-winners-test)
(5am:run! 'adjust-test)
(5am:run! 'all-tests)

(5am:def-suite all-tests :description "cl-poker tests")
(5am:in-suite all-tests)

(5am:test adjust-test
  (let* ((bet-list (list (cons 100
			      (list (cons (make-player "fyv") 50)
				    (cons (make-player "wag") 100)))))
	(winners (list (car (first (cdr (car bet-list))))
		       (car (second (cdr (car bet-list)))))))
    (adjust winners bet-list)
    (5am:is (= (get-bet (car winners) (car bet-list)) 0))
    (5am:is (= (get-bet (cadr winners) (car bet-list)) 0)))
  (let* ((bet-list (list (cons 100
			      (list (cons (make-player "fyv") 50)
				    (cons (make-player "wag") 100)))))
	 (winners (list (car (first (cdr (car bet-list)))))))

    (adjust winners bet-list)
    (print bet-list)
    (5am:is (= (get-bet (car winners) (car bet-list)) 0))
    (5am:is (= (get-bet (car (second (cdr (car bet-list)))) (car bet-list)) 50))))

(5am:test pay-winners-test
  (let* ((list-of-winners (list (make-player "fyv") (make-player "wag")))
	 (bet-list (list (cons 100 (mapcar #'(lambda (x) (cons x 100))
					   list-of-winners)))))
    (pay-winners list-of-winners bet-list)
    (5am:is (= (payout (car list-of-winners)) 100))
    (5am:is (= (payout (cadr list-of-winners)) 100))))

(5am:test adjust-bets-test
  (let ((bet-list (list 20 (cons (make-player "wag") 30)
			(cons (make-player "fyv") 20))))
    (5am:is (= (get-bet (caar (last bet-list)) bet-list) 20))
    (adjust-bets (caar (last bet-list)) bet-list)
    (5am:is (= (get-bet (caadr bet-list) bet-list) 10))))

(5am:test get-bet-test
  (let* ((p (make-player "fyv"))
	 (bet-list (list 20 (cons (make-player "wag") 30) (cons p 20))))
    (5am:is (= (get-bet p bet-list) 20))
    (5am:is (= (get-bet (car (cadr bet-list)) bet-list) 30))
    (5am:is (not (get-bet (make-player "jane") bet-list)))))

(5am:test advance-acting-test
  (let* ((p (make-player "fyv"))
	 (players (list
		   (make-player "wag")
		   p
		   (make-player "gog"))))
    (setf (acting p) t)
    (advance-acting players)
    (5am:is (acting (car (last players))))
    (setf (folded (car (last players))) t)
    (advance-acting players)
    (5am:is (acting (car players)))))

(5am:test get-acting-test
  (let ((q (list
	    (make-player "fyv")
	    (make-player "wag"))))
    (setf (acting (car q)) t)
    (5am:is (get-acting q))))

(5am:test player-in-bets?-test
  (let ((p (make-player "gog")))
    (setf *bets*
	  (list 500
		(cons p 500)
		(cons (make-player "fyv") 500)
		(cons (make-player "wag") 500)))
    (5am:is (equal (cons p 500) (player-in-bets? *bets* p)))))

(5am:test handle-player-action-tests-bets
  (let ((p (make-player "fyv")))
    (setf *bets* nil)
    (handle-player-action p 'bet 100)
    ;; value at head of bet-list was updated
    (5am:is (= (car *bets*) 100))
    ;; player and his bet are recorded in bet-list
    (5am:is (equal (cadr *bets*) (cons p 100))))
  (let ((p (make-player "fyv")))
    (setf *bets* nil)
    (handle-player-action p 'raise 100)
    ;; player raises when he should bet.
    (5am:is (not (null *bets*)))))

(5am:test handle-player-action-tests-raises
  (let ((p (make-player "wag")))
    (setf *bets* (list 200 (cons (make-player "fyv") 200)))
    ;; too small a is rejected
    (5am:is (null (handle-player-action p 'raise 150)))
    (handle-player-action p 'raise 400)
    ;; raise is recorded at head of *bets*
    (5am:is (= (car *bets*) 400))
    ;; player and his raise are recoreded in *bets*
    (5am:is (= (cdr (car (last *bets*))) 400)))
  (let ((p (make-player "fyv")))
    (setf *bets* (list 100 (cons p 100)))
    (handle-player-action p 'bet 200)
    ;; a player bets when he should raise.
    (5am:is (= (car *bets*) 200))))

(5am:test handle-player-action-tests-calls
  (let ((p (make-player "wag")))
    (setf *bets* (list 200 (cons (make-player "fyv") 200)))
    (handle-player-action p 'call)
    ;; check whether calling player (who is not in bet-list) is there
    ;; once he calls.
    (5am:is (equal p (car (player-in-bets? *bets* p)))))
  (let ((p (make-player "wag")))
    ;; check whether the player is in *bets* given that he was before.
    (setf *bets* (list 200 (cons (make-player "fyv") 200)
  		       (cons p 100)))
    (handle-player-action p 'call)
    (5am:is (equal p (car (player-in-bets? *bets* p))))
    ;; check whether player's bet was updated
    (5am:is (= (cdr (player-in-bets? *bets* p)) 200))))

(5am:test call-tests
  (let ((p (make-player "gog")))
    (5am:is (= (cdr (call p 100)) 100))
    (5am:is (= (chips p) 900)))
  (let ((p (make-player "wag")))
    (5am:is (= (cdr (call p 1000)) 1000))
    (5am:is (= (chips p) 0))))

(5am:test bet-tests
  (let ((p (make-player "wag")))
    (bet p 2000)
    (5am:is (= (chips p) 0)))
  (let ((p (make-player "wag")))
    (bet p 500)
    (5am:is (= (chips p) 500)))
  (let* ((p (make-player "wag"))
  	 (return-val (bet p 100)))
    (5am:is (= (car return-val) 100))
    (5am:is (equal (caadr return-val) p))))

(5am:test player-tests
  (let ((p (make-player "fyv")))
    (5am:is (string= (pname p) "fyv"))
    (5am:is (= (chips p) 1000))
    (setf (chips p) 2000)
    (5am:is (= (chips p) 2000))))
