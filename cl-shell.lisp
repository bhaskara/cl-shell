;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for writing shell scripts in common lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :cl-shell
  (:export
   :with-dir
   :shell-command-error
   :do-lines
   :tokenize
   :run-external)
  (:use :cl :sb-ext))

(in-package :cl-shell)

(defun run-external (cmd &rest args)
  "run-external CMD &rest ARGS.  

Blocking call to external command.   Return standard output contents assuming 0 return value.  If nonzero return value, throws an error."
  
  (let* ((str (make-string-output-stream))
	 (error-str (make-string-output-stream))
	 (proc (sb-ext:run-program cmd args :search t :output str :error error-str :wait nil)))
    (sb-ext:process-wait proc)
    (if (zerop (sb-ext:process-exit-code proc))
	(get-output-stream-string str)
      (error "Received exit code ~a when running external process ~a with args ~a.~&Standard error output was ~a"
	     (sb-ext:process-exit-code proc) cmd args (get-output-stream-string error-str)))))


(defmacro do-lines ((line-var filename) &body body)
  "do-lines (L FILENAME) &body BODY

L is an unevaluated symbol.
FILENAME is either a string or path, in which case it is taken to name a file, or another form, which is then evaluated and treated as the _contents_ of a file.

Repeatedly execute body with variable L bound to string containing successive lines of the file."
  
  (let ((f (gensym)))
    `(,@(stream-binding-form f filename)
	(loop
	   (let ((,line-var (read-line ,f nil)))
	     (unless ,line-var (return))
	     ,@body)))))


(defun stream-binding-form (f filename)
  (typecase filename
    ((or string pathname) `(with-open-file (,f ,filename :direction :input)))
    (t `(let ((,f (make-string-input-stream ,filename)))))))
      



(defun tokenize (str &optional (separators '(#\Space #\Tab #\Newline)))
  "Tokenize string using separators and return list of tokens"
  (nreverse (tokenize-helper (string-trim '(#\Space #\Tab #\Newline) str) nil separators)))

(defun tokenize-helper (str l bag)
  (let ((pos (position-if #'(lambda (c) (member c bag)) str)))
    (if pos
	(tokenize-helper
	 (subseq str (position-if #'(lambda (c) (not (member c bag))) str :start pos))
	 (cons (subseq str 0 pos) l)
	 bag)
	(cons str l))))

