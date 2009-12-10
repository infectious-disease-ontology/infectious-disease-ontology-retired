(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defun translate-pathway ()
  (get-sheets :file "ido:immunology;ido-s4lps-tlr4.xls"))

(defun find-block (headers sheet)
  (loop for row in sheet
     with collecting
     for found = (or found (search headers row :test 'equalp))
;     do (print-db found collecting row)
     when (and (eq collecting :found) (not (null (nth found row)))) do (setq collecting :collect)
     when (and found (null collecting)) do (setq collecting :found) 
     until (and (eq collecting :collect) (null (nth found row)))
     when (eq collecting :collect) collect (mapcar (lambda(e)
						     (if (stringp e) (string-trim " " e) e))
						   (subseq row found (+ found (length headers))))
     ))

(defparameter *blocks*
  '((:handles "handle" "Entities" "Kind" "class"	"super(s)")
    (:processes "processes" "realizes" "part_of" "class" "super(s)" "binding domains")
    (:complexes "complexes without process forming them" "has_part")
    ))

(defun locate-blocks-in-sheets (file types &optional within-sheets)
  (loop for (sheet-name sheet) in (list-sheets :file file)
     for found = (and (or (null within-sheets) (member sheet-name within-sheets :test 'equalp))
		      (loop for type in types
			 for found = (find-block (cdr (assoc type *blocks*))
						 (get-sheet-as-row-lists sheet))
			 when found collect (list type found)))
     when found collect      
     (list sheet-name found)))

;(locate-blocks-in-sheets "ido:immunology;ido-s4lps-tlr4.xls" '(:handles :processes) '("TLR4MyD88"))

(defun check-processes-use-only-defined-handles (complexes handles processes)
  (loop for (process) in processes
       for parsed = (parse-process process)
       when (eq (car parsed) :simple)
       do
       (print parsed)
       (loop for term in (append (second parsed) (third parsed))
	    when (not (or (find term handles :key 'car :test 'equalp) (find term complexes :key 'car :test 'equalp)))
	    do (format t "~%Did't find ~a from ~a~%" term process))))

(defun parse-process (string)
  ;; x + y + x -> a + b
  (setq string (string-trim " " string))
  (cond ((#"matches" string "^(([A-Za-z0-9-]+?)\\s*\\+\\s*)*([A-Za-z0-9-]+?)\\s*(->)\\s*(([A-Za-z0-9-]+?)\\s*\\+\\s*)*([A-Za-z0-9-]+?)\\s*$")
	 (list* :simple (mapcar (lambda(e) (split-at-regex e "\\s*\\+\\s*"))
			       (split-at-regex string "\\s*->\\s*"))))
	(t (list :notsimple string))))

(defun test ()
  (let ((found (locate-blocks-in-sheets "ido:immunology;ido-s4lps-tlr4.xls" '(:handles :processes :complexes))))
    
    (let ((handles (loop for (sheet blocks) in found append (second (assoc :handles blocks))))
	  (processes (append (second (assoc :processes (second (car found))))))
	  (complexes (loop for (sheet blocks) in found append (second (assoc :complexes blocks)))))
      (check-processes-use-only-defined-handles complexes handles processes))))