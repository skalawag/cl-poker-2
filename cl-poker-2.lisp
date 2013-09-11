;;;; cl-poker-2.lisp

(in-package #:cl-poker-2)

(defclass player ()
  ((player-name
    :initarg :name
    :reader pname)
   (chips
    :initarg :chips
    :accessor chips)
   (pocket-cards
    :accessor pockets)
   (best-hand
    :initform nil
    :accessor hand)
   (acting-player
    :initform nil
    :accessor acting)
   (folded-player
    :initform nil
    :accessor folded)
   (big-blind
    :initform nil
    :accessor bb)
   (small-blind
    :initform nil
    :accessor sb)
   (chips-at-line
    :initform 0
    :accessor bett)
   (player-payout
    :initform 0
    :accessor payout)
   (round-over-marker
    :initform nil
    :accessor flag)))

(defun make-player (name)
  (make-instance 'player :name name :chips 1000))

(defun bet (player amt)
  (cond
    ((> amt (chips player))
     (let ((amt (chips player)))
       (setf (bett player) (chips player))
       (setf (chips player) 0)
       (list amt (cons player amt))))
    (t
     (setf (chips player) (- (chips player) amt))
     (setf (bett player) amt)
     (list amt (cons player amt)))))

(defun call (player amt)
  (cond
    ((< (chips player) amt)
     (let ((new-amt (chips player)))
       (setf (bett player) new-amt)
       (setf (chips player) 0)
       (cons player new-amt)))
    (t
     (let ((p (player-in-bets? *bets* player)))
       (if p
	   (progn
	     (setf (chips player) (- (chips player) (- amt (cdr p))))
	     (setf (bett player) amt))
	   (progn
	     (setf (bett player) amt)
	     (setf (chips player) (- (chips player) amt))))
       (cons player amt)))))

(defun raise (player amt)
  (cadr (bet player amt)))

(defparameter *prev-bets* nil
  "After each betting round, *bets* are simply pushed here.")

(defparameter *bets* nil
  "Collect all bets for each betting round.")

(defun player-in-bets? (bet-list player)
  "If a player has already placed a bet or called, retrieve him from
*bets*"
  (labels ((get-p (bets)
	     (cond
	       ((null bets) nil)
	       ((not (consp (car bets)))
		(get-p (cdr bets)))
	       ((equal player (caar bets))
		(car bets))
	       (t (get-p (cdr bets))))))
    (get-p bet-list)))

(defun update-player-in-bets (player-bet bet-list)
  (append
   (list (car bet-list))
   (remove-if #'(lambda (y) (equal y (car player-bet)))
	      (cdr bet-list) :key #'(lambda (x) (car x)))
   (list player-bet)))

(defun handle-player-action (player action &optional amt)
  (cond
    ((eq action 'fold)
     (setf (folded player) t))
    ((eq action 'call)
     (let ((p (player-in-bets? *bets* player)))
       (if p
	   (setf *bets* (update-player-in-bets
			   (call player (car *bets*)) *bets*))
	   (setf *bets*
		 (append *bets* (list (call player (car *bets*))))))))
    ((eq action 'bet)
     (if *bets*
	 (handle-player-action player 'raise amt)
	 (progn
	   (setf *bets* (bet player amt))
	   (set-player-flag player))))
    ((eq action 'raise)
     (cond
       ((null *bets*)
	(handle-player-action player 'bet amt))
       ((< amt (* 2 (car *bets*)))
	nil) ; illegal raise
       (t
	(set-player-flag player)
	(setf *bets* (append *bets* (list (raise player amt))))
	(setf (car *bets*) amt))))))

(defun set-player-flag (player)
  "When a player bets or raises, set this flag. When we see it again,
the betting-round is over."
  (dolist (p players)
    (setf (flag p) nil))
  (setf (flag player) t))

(defun betting-round-over? (player)
  "When getting the next player to act, check his flag. If it is true,
  then he was the original bettor (or the last raiser) and no one has
  reraised; the round is over."
  (when (flag player) t))

(defun hand-over? (player)
  (when (or (and (eq (flag player) t) (eq *stage* "River"))
	    (<
	     (length
	      (remove-if #'(lambda (p) (= (chips p) 0))
			 (get-unfolded *players*)))
	     2))))

(defun inc-player-bet (player)
  (dolist (p *bets*)
    (when (and (consp p) (equal (car p) player))
      (setf (bett (car p)) (cdr p)))))

(defun update-bets ()
  (mapcar #'inc-player-bet *players*))

(defun compute-pot (prev-bets bets)
    (reduce #'+ (remove-if-not #'consp (append prev-bets bets))
	    :key #'(lambda (x) (cdr x))))

(defparameter *players* nil)

(defparameter *community-cards* nil)

(defun deal-cards ()
  (let ((cards
	 (subseq (cl-cards:shuffle) 0 (+ (* 2 (length *players*)) 5))))
    (dolist (p *players*)
      (setf (pockets p)
	    (list
	     (pop cards)
	     (pop cards))))
    (setf *community-cards* cards)))

(defun rotate-players (players)
  (append (list (car (last players))) (butlast *players*)))

(defun clear-blinds (players)
  (cond
    ((null players) nil)
    (t (setf (sb (car players)) nil)
       (setf (bb (car players)) nil)
       (clear-blinds (cdr players)))))

(defun set-blinds (players)
  "Always call this after rotating, and after clear-blinds."
  (setf (sb (car players)) t)
  (handle-player-action (car players) 'bet 5)
  (setf (bb (cadr players)) t)
  (handle-player-action (cadr players) 'raise 10))

(defun get-acting (players)
  (let ((acting (remove-if-not #'(lambda (p) (acting p)) players)))
    (if (> (length acting) 1)
	(error "More than one acting player")
	(car acting))))

(defun get-unfolded (players &optional ignore-acting)
  (if ignore-acting
      (remove-if
       #'(lambda (x) (and (folded x) (not (acting x)))) players)
      (remove-if
       #'(lambda (x) (folded x)) players)))

(defun advance-acting (players)
  (let ((active (get-unfolded players t)))
    (cond
      ((acting (car (last active)))
       (setf (acting (car (last active))) nil)
       (setf (acting (car active)) t))
      (t
       (labels ((move-acting (source)
		  (cond
		    ((null source) nil)
		    ((acting (car source))
		     (setf (acting (car source)) nil)
		     (setf (acting (cadr source)) t))
		    (t (move-acting (cdr source))))))
	 (move-acting active))))))

(defun reset-hand ()
  (mapcar #'(lambda (x)
	      (setf (acting x) nil)
	      (setf (folded x) nil)
	      (setf (bett x) 0)) *players*)
  (clear-blinds *players*)
  (setf *players* (rotate-players *players*))
  (setf (acting (third *players*)) t)
  (setf *bets* nil)
  (setf *prev-bets* nil)
  (set-blinds *players*)
  (deal-cards))

(defparameter *stage* "Pre-Flop")

(defun advance-stage ()
  (cond
    ((string= *stage* "Pre-Flop") (setf *stage* "Flop"))
    ((string= *stage* "Flop") (setf *stage* "Turn"))
    ((string= *stage* "Turn") (setf *stage* "River"))
    ((string= *stage* "River") (setf *stage* "Pre-flop"))))

;;; call eval-hands and then get-winners, maybe combine these two.
(defun eval-hands (players)
  (dolist (p players)
    (setf (hand p) (hand-evaluator:best-poker-hand
		    (append (pockets p) *community-cards*)))))

(defun get-winners (players)
  "Return a list of the players with the highest value hands"
  (let ((best 0))
    (dolist (p players)
      (when (> (car (hand p)) best)
	(setf best (car (hand p)))))
    (remove-if #'(lambda (x) (< x best)) players
	       :key #'(lambda (p) (car (hand p))))))

(defun sort-players-by-hand-val (players)
  (sort (copy-list players) #'> :key (lambda (p) (car (hand p)))))

(defun get-player-payout (player bets)
  "bets is a single betlist of all bets"
  (let ((payout 0))
    (dolist (b (cdr bets))
      (let ((check (if (equal (car b) player) b nil)))
	(when check
	  (setf payout
		(+ payout (* (cdr check) (- (length bets) 1))))
	  (return payout))))
    payout))

(defun remove-player-from-all-betlists (player)
  (setf *bets* (remove-if #'(lambda (x) (if (and (consp x) (equal (car x) player)) t nil)) *bets*))
  (setf *prev-bets* (remove-if #'(lambda (x) (if (and (consp x) (equal (car x) player)) t nil)) *prev-bets*)))

(defun get-bet (player bet-list)
  (dolist (b bet-list)
    (when (consp b)
      (if (equal (car b) player)
	  (return (cdr b))))))

(defun adjust-bets (player bet-list)
  (let ((bet (get-bet player bet-list)))
    (if bet
	(dolist (b (cdr bet-list))
	  (setf (cdr b) (- (cdr b) bet))))))

;; TODO: this needs lots of tests
(defun calculate-player-payouts (winners players bets prev-bets)
  (let ((bl (copy-list bets))
	(pb (copy-list prev-bets)))
    (dolist (w winners)
      (let ((amt 0))
	(dolist (bet-list (push bl pb))
	  (setf amt (+ amt (get-player-payout w bet-list))))
	(setf (payout w) (/ amt (length winners)))))
    (dolist (w winners)
      (remove-player-from-all-betlists w))
    (dolist (b (push bl pb))
      (adjust-bets (car (sort (copy-list winners) #'> :key #'cdr)) b))
    (when (and (> (compute-pot prev-bets bets) 0) (not (null players)))
      (let ((new-players
	     (remove-if
	      #'(lambda (p) (member p winners :test #'equal)) players)))
	(calculate-player-payouts (get-winners new-players)
				  new-players
				  bets
				  prev-bets)))))

(defun credit-payouts (players)
  "Move chips into players chip stack from payout."
  (dolist (p players)
    (setf (chips p) (+ (chips p) (payout p)))
    (setf (payout p) 0)))


;;;;;;;;;;;;;;;;;;;;;;;
;;;; testing and such
;;;;;;;;;;;;;;;;;;;;;;;
(defun make-players ()
  (let ((names '("fyv" "wag" "gog" "hal" "tip" "mary")))
    (dolist (n names)
      (push (make-player n) *players*))))

(defun seating-format-values ()
  (let ((n (- (length *players*) 3))
	(res (list "[sb]" "[bb]")))
    (dotimes (i n)
      (setf res (append res (list (+ i 3)))))
    (append res (list "[d]"))))

(defun hline ()
  (format t "---------------------------------------~%"))

(defun display ()
  (let ((seats (seating-format-values)))
    (hline)
    (format t "Stage: ~9a Pot: ~6a~%"
	    *stage*
	    (compute-pot *prev-bets* *bets*))
    (hline)
    (format t "~5a ~10a ~4a ~7a ~4a ~3a~%"
	    "Seat" "Name" "Nxt" "Chips" "Bet" "Fld")
    (hline)
    (dolist (p *players*)
      (format t "~5a ~10a ~4a ~7a ~4a ~3a~%"
	      (pop seats)
	      (pname p)
	      (if (acting p) "*" "")
	      (chips p)
	      ;; not sure how best to fix this yet.
	      (if (null (get-bet p *bets*)) 0 (get-bet p *bets*))
	      (if (folded p) (folded p) "--")))))

(defun clear ()
  (setf *players* nil)
  (setf *bets* nil)
  (setf *prev-bets* nil)
  (setf *community-cards* nil))

(defun setup ()
  (make-players))

(defun reset (&optional run)
  (clear)
  (setup)
  (reset-hand)
  (when run
    (funcall run))
  (display))
