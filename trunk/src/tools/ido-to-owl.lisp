; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)
;; (load "~/lsw/biopax/proto/obo.lisp")

(defun ido-to-owl (&key (file "ido:ido-core;IDO-editors-worksheet.xls"))
  (let* ((xls (new 'hssf (namestring (truename file))))
	 (workbook (get-java-field xls "hssfworkbook" t))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (termdefs (loop for sheet in sheets
		      when (member (car sheet) 
				   '("Roles" "Dispositions" "Qualities" "Processes" "Objects"	 
				     "Object Aggregates" "Temporal Intervals" "Sites" "Defined Classes")
				   :test 'equal)
		      append (get-sheet (second sheet) (first sheet)))))
    (setq @ termdefs)
    (setq @@ sheets)
    (print-db (check-isas termdefs))
					;(check-historical termdefs)
    ))

(defun get-sheet (sheet sheet-name)
  (destructuring-bind (headers . rows)
      (loop for rowno below (#"getPhysicalNumberOfRows" sheet)
	 with nocells = (loop for row below (#"getPhysicalNumberOfRows" sheet)
			   maximize (or (and (not (#"getRow" sheet row)) 0)
					(#"getPhysicalNumberOfCells" (#"getRow" sheet row))))
	 for row = (#"getRow" sheet rowno)
	 for potential = (and row 
			      (list sheet-name rowno
				    (loop for colno below nocells
				       for cell = (#"getCell" row colno)
				       collect (and cell (#"toString" cell)))))
	 for thereyet = (or thereyet (member "Term" (third potential) :test 'equalp))
	 when (and thereyet row) collect potential)
    (let ((headerkeys (mapcar (lambda(s)(intern (string-upcase s)'keyword)) (third headers))))
      (loop for (sheet rowno row) in rows
	 collect
	   (append `((:sheet ,sheet) (:row ,(1+ rowno)))
		   (loop for key in headerkeys
		      for cell in row
		      if (and (equal key :synonym) cell)
		      do (if (equal cell "")
			       (setq cell nil)
			       (setq cell (mapcar (lambda(e) (string-trim " " e)) (split-at-regex cell "[;,]"))))
		      collect (list key cell)))))))

(defun check-isas (termdefs)
  (let ((terms 
	 (loop for entry in termdefs
	    for term = (second (assoc :term entry))
	    for isa = (second (assoc :is_a entry))
	    for inheresin = (second (assoc :inheres_in entry))
	    when (and entry term (not (equal term "")))
	    when (and (find :is_a entry :key 'car)
		      (null isa))
	    do (warn "Missing is-a ~a" entry)
	    collect (list term isa)
	    when inheresin
	    collect (list term )
	      )))
    (remove-duplicates (set-difference (mapcar 'second terms) (mapcar 'first terms) :test 'equalp) :test 'equalp)))

(defun check-historical (termdefs)
  (let ((historical (historical-ido-terms)))
    (map nil 'print
	 (loop for entry in termdefs
	    for term = (second (assoc :term entry))
	    for id = (second (assoc :id entry))
	    when (and id entry (not (equal term "")) (not (equal id "")) (second (assoc id historical :test 'equal)))
	    collect (list id term (second (assoc id historical :test 'equal)))))
    (map nil 'princ (mapcar 'cdr (sort 
		     (loop for (id name) in historical
			for in-sheet = (find-if (lambda(e) (or (equal name (second (assoc :term e)))
							       (member name (second (assoc :synonym e)) :test 'equal)))
						termdefs)
			if in-sheet 
			collect (cons name
				      (format nil "historical ~a : ~s -> ~a - ~a row ~a~%" id name (or (second (assoc :id in-sheet)) "no id")
					      (second (assoc :sheet in-sheet))
					      (second (assoc :row in-sheet))))
			else collect (cons name
					   (format nil "historical ~a : ~s - no mapping, deprecate?~%" id name))
			) 'string-lessp :key 'car)))
    ))

(defun historical-ido-terms ()
  (let ((kb (load-kb-jena "ido:ido-core;historical;IDO-1-3-oboconv.owl")))
    (let ((*current-labels* (rdfs-labels kb)))
      (mapcar (lambda(e)
		(list (#"replaceAll" (#"replaceAll" (uri-full (first e)) ".*#" "") "_" ":")
		      (second e)))
	      (loop for class in
		   (remove-if-not (lambda(e) (search "IDO#" (uri-full e)))
				  (descendants !owl:Thing kb))
		   collect (list class (gethash class *current-labels*)))))))

(defun ido-2008-terms ()
  (let ((obo (make-instance 'obo :path "ido:ido-core;historical;IDO-2009-05-15.obo")))
    (read-obo obo)
    (loop for record in (terms obo) 
       for id = (getf (cdr record) :id)
       for name = (getf (cdr record) :name)
       when (search "IDO:" id)
       collect (list id name))))

(defparameter *ido-external-terms*
  (eval-uri-reader-macro
   '(("object aggregate"  !<http://www.ifomis.org/bfo/1.1/snap#ObjectAggregate>)
					;"active immunization against smallpox" 
     ("process" !span:Process)
     ("disease course(OGMS:0000063)" !obo:OGMS_0000063)
     ("material entity that is alive" !obo:OBI_0100026) ; organism
     "organism that has the capability to bear the host role" 
     ("material entity" !<http://www.ifomis.org/bfo/1.1/snap#MaterialEntity>)
     ("organism"   !obo:OBI_0100026)
     "organism that is the bearer of the host of infectious parasite role" 
     "organism that is the bearer of the host role" 
     "organism that is the bearer of the host of infectious agent role" 
     "organism with an immune system"
     "organism that has the pathogenic disposition" 
     "organism that has the infectious disposition" 
     ("quality"  !<http://www.ifomis.org/bfo/1.1/snap#Quality>)
     ("disposition"  !<http://www.ifomis.org/bfo/1.1/snap#Disposition>)
     ("disease" !obo:OGMS_0000063)
     ("role" !<http://www.ifomis.org/bfo/1.1/snap#Role>))))