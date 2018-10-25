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
	
	...
