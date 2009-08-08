; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)

(defun ido-to-owl (&key (file "ido:ido-core;IDO-term-list.xls"))
  (let* ((xls (new 'hssf (namestring (truename file))))
	 (workbook (get-java-field xls "hssfworkbook" t))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		      collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (termdefs (loop for sheet in sheets append (get-sheet (second sheet) (first sheet)))))
    (setq @ termdefs)
    (print-db (check-isas termdefs))
    (check-historical termdefs)))

(defun get-sheet (sheet sheet-name)
  (destructuring-bind (headers . rows)
      (loop for rowno below (#"getPhysicalNumberOfRows" sheet)
	 for row = (#"getRow" sheet rowno)
	 when row
	 collect
	 (list sheet-name rowno
	       (loop for colno below (#"getPhysicalNumberOfCells" row)
		  for cell = (#"getCell" row colno)
		  collect (and cell (#"toString" cell)))))
    (let ((headerkeys (mapcar (lambda(s)(intern (string-upcase s)'keyword)) (third headers))))
      (loop for (sheet rowno row) in rows
	 collect
	   (append `((:sheet ,sheet) (:row ,(1+ rowno)))
		   (loop for key in headerkeys
		      for cell in row
		      collect (list key cell)))))))

(defun check-isas (termdefs)
  (let ((terms 
	 (loop for entry in termdefs
	    for term = (second (assoc :term entry))
	    for isa = (second (assoc :is_a entry))
	    for inheresin = (second (assoc :inheres_in entry))
	    when (and entry (not (equal term "")))
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
    (loop for (id name) in historical
       for in-sheet = (find name termdefs :key (lambda(e) (second (assoc :term e)))  :test 'equal)
       when in-sheet 
       do 
       (format t "historical ~a : ~s -> ~a - ~a row ~a~%" id name (or (second (assoc :id in-sheet)) "no id")
	       (second (assoc :sheet in-sheet))
	       (second (assoc :row in-sheet)))
       )))

(defun historical-ido-terms ()
  (let ((kb (load-kb-jena "ido:ido-core;historical;IDO.owl")))
    (let ((*current-labels* (rdfs-labels kb)))
      (mapcar (lambda(e)
		(list (#"replaceAll" (#"replaceAll" (uri-full (first e)) ".*#" "") "_" ":")
		      (second e)))
	      (loop for class in
		   (remove-if-not (lambda(e) (search "IDO#" (uri-full e)))
				  (descendants !owl:Thing kb))
		   collect (list class (gethash class *current-labels*)))))))
	    

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