(defstruct hmm
  (states '())
  
  (n 0 :type fixnum)
  (transitions (make-hash-table :test #'equal))
  (emissions (make-hash-table :test #'equal))
  (states-observations (list :states (make-hash-table :test #'equal)
			     :observations (make-hash-table :test #'equal)))  ;; trenger egentlig bare totalen av states her
  (id (make-hash-table :test #'equal))
  (lab (make-hash-table :test #'equal)))
  
(defun transition-probability (hmm id1 id2)
  (gethash (list id2 id1)
	   (hmm-transitions hmm)))

(defun emission-probability (hmm id1 word)
  (gethash (list word id1)
	   (hmm-emissions hmm)))
     
(defun state2id (hmm lab)
  (if (not (gethash lab (hmm-id hmm)))
      (progn (incf (hmm-n hmm))
	     (setf (gethash lab (hmm-id hmm))
		   (hmm-n hmm))
	     (setf (gethash (hmm-n hmm) (hmm-lab hmm))
		   lab)
	     (setf (hmm-states hmm) (cons lab (hmm-states hmm)))
	     (hmm-n hmm))
      (gethash lab (hmm-id hmm))))
      
(defun break-line (l)
  (loop for start = 0 then (+ space 1)
       for space = (position #\tab l :start start)
       for token = (subseq l start space)
       collect token
       until (not space)))

(defun read-corpus(file n)
(let* ((hmm (make-hmm))

	   (bigramlist
	    (with-open-file (stream file :direction :input)
	      (loop for line = (read-line stream nil)
		 while line
		collect (break-line line)))))

  (incf (gethash (list (state2id hmm (cadar bigramlist)) (state2id hmm "<s>")) (hmm-transitions hmm) 0))
  (incf (gethash (list (caar bigramlist) (state2id hmm (cadar bigramlist))) (hmm-emissions hmm) 0))
  (incf (gethash (state2id hmm "<s>") (getf (hmm-states-observations hmm) :states) 0))
  
  (loop for bi in bigramlist
     for i from 0

     do (if (not (equal (car bi) ""))
	    (progn (incf (gethash (state2id hmm (cadr bi)) (getf (hmm-states-observations hmm) :states) 0))
		   (incf (gethash (car bi) (getf (hmm-states-observations hmm) :observations) 0)))
	    (progn (incf (gethash (state2id hmm "<s>") (getf (hmm-states-observations hmm) :states) 0))
		   (incf (gethash (state2id hmm "</s>") (getf (hmm-states-observations hmm) :states) 0))))

     when (and (not (equal (car bi) "")) (not (eq i 0)))

     do (cond ((not (nth (+ i 2) bigramlist))
	       (progn (incf (gethash (list (car bi) (state2id hmm (cadr bi))) (hmm-emissions hmm) 0))
		      (incf (gethash (list (state2id hmm (cadr bi)) (state2id hmm (cadr (nth (- i 1) bigramlist)))) (hmm-transitions hmm) 0))
		      (incf (gethash (list (state2id hmm "</s>") (state2id hmm (cadr bi))) (hmm-transitions hmm) 0))))
			    
	      (t (progn (incf (gethash (list (car bi) (state2id hmm (cadr bi))) (hmm-emissions hmm) 0))
			(if (not (equal (car (nth (- i 1) bigramlist)) ""))
			    (incf (gethash (list (state2id hmm (cadr bi)) (state2id hmm (cadr (nth (- i 1) bigramlist)))) (hmm-transitions hmm) 0))

			    (progn (incf (gethash (list (state2id hmm "</s>") (state2id hmm (cadr (nth (- i 2) bigramlist)))) (hmm-transitions hmm) 0))
				   (incf (gethash (list (state2id hmm (cadr bi)) (state2id hmm "<s>")) (hmm-transitions hmm) 0))))))))
  
  hmm))
  
(defun train-hmm (hmm)
  (loop for k being the hash-keys of (hmm-transitions hmm) using (hash-value v)
     do (setf (gethash k (hmm-transitions hmm)) (/ v (gethash (cadr k) (getf (hmm-states-observations hmm) :states)))))
  (loop for k being the hash-keys of (hmm-emissions hmm) using (hash-value v)
     do (setf (gethash k (hmm-emissions hmm)) (/ v (gethash (cadr k) (getf (hmm-states-observations hmm) :states)))))
  hmm)

(defun viterbi (hmm seq) 
  (let* ((n (length seq))
	 (l (length (hmm-states hmm)))
	 (max (coerce 0 'float))
	 (maxlast 0)
	 (path-prob (make-hash-table))
	 (path-backpointer (make-hash-table)))

	 (loop for i from 1 to (+ n 1)
	    do (setf (gethash i path-prob) (make-hash-table))
	    do (setf (gethash i path-backpointer) (make-hash-table)))
	 
	 (loop for s in (hmm-states hmm) 
	    when (and (not (equal s "</s>")) (not (equal s "<s>")))
	    do (progn (setf (gethash (state2id hmm s) (gethash 1 path-prob))
			    (* (or (gethash (state2id hmm "<s>") (gethash (state2id hmm s) (hmm-transitions hmm))) 1/1000000)
			       (or (gethash (state2id hmm s) (gethash (car seq) (hmm-emissions hmm))) 1/1000000)))
		      (setf (gethash (state2id hmm s) (gethash 1 path-backpointer)) (state2id hmm "<s>"))))

	 (loop for i from 2 to n
	    do (loop for s in (hmm-states hmm) 
		  do (setf max 0) 
		  do (loop for k being the hash-keys of (gethash (- i 1) path-prob) using (hash-value v) 
			do (when (and (not (eql k (state2id hmm "</s>"))) (not (equal s "</s>")) (not (equal s "<s>")))
			     (when (> (* v (or (gethash k (gethash (state2id hmm s) (hmm-transitions hmm))) 1/1000000)
					 (or (gethash (state2id hmm s) (gethash (nth (- i 1) seq) (hmm-emissions hmm))) 1/1000000)) max)
			       (setf (gethash (state2id hmm s) (gethash i path-prob)) (setf max (* v (or (gethash k (gethash (state2id hmm s) (hmm-transitions hmm))) 1/1000000)
												   (or (gethash (state2id hmm s) (gethash (nth (- i 1) seq) (hmm-emissions hmm))) 1/1000000))))
			       (setf (gethash (state2id hmm s) (gethash i path-backpointer)) k))))))

	 
	 (loop for s in (hmm-states hmm)
	      when (and (not (equal s "<s>")) (not (equal s "</s>")))

	    do (when (> (* (gethash (state2id hmm s) (gethash n path-prob)) (or (gethash (state2id hmm s) (gethash (state2id hmm "</s>") (hmm-transitions hmm))) 1/1000000)) maxlast)
		 (setf (gethash (state2id hmm "</s>") (gethash (+ n 1) path-prob))
		       (setf maxlast (* (gethash (state2id hmm s) (gethash n path-prob)) (or (gethash (state2id hmm s) (gethash (state2id hmm "</s>") (hmm-transitions hmm))) 1/1000000))))
		 (setf (gethash (state2id hmm "</s>") (gethash (+ n 1) path-backpointer)) (state2id hmm s))))

	 (let* ((end (gethash (state2id hmm "</s>") (gethash (+ n 1) path-backpointer))))
	   (reverse (append (list (gethash end (hmm-lab hmm)))
		   (loop for i from n downto 2
		      do (setf end (gethash end (gethash i path-backpointer)))
		      collect (gethash end (hmm-lab hmm))))))))
