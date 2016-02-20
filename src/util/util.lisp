(defpackage :util
  (:use :common-lisp)
  (:export :read-file-to-string
	   :take
	   :period-split
	   :intersperse
	   :term-to-string
	   :print-term))

(in-package :util)

(defun read-file-to-string (path)
  (with-open-file (stream path) ; TODO: fehler falls datei nicht existiert
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun take (n l)
  (cond ((< (length l) n) l)
	(t (subseq l 0 n))))

(defun period-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\. string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun intersperse (obj ls)
  (cons (car ls) (intersperse-rec obj (reverse (cdr ls)) nil)))

(defun intersperse-rec (obj ls acc)
  (if ls
      (intersperse-rec obj (cdr ls) (cons obj (cons (car ls) acc)))
     acc))

;; ToDo:
;;   - Behandlung von Listen
;;   - Behandlung von arithmetischen Ausdrücken
;;   - Behandlung von Zahlen
(defun term-to-string (term)
  (let ((termtype (get term 'type)))
    (cond ((or (equal termtype 'variable)
	       (equal termtype 'const)) (get term 'name))
	  (T (concatenate 'string (get term 'name)
			  "("
			  (eval (append '(concatenate 'string)
					(intersperse "," (mapcar 'term-to-string
								 (get term 'args)))))
			  ")")))))

(defun print-term (term)
  (print (term-to-string term)))
