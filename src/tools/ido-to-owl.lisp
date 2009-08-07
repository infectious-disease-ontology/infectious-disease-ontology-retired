; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)

(defun ido-to-owl (&key (file "/Users/alanr/Downloads/2009-07-26/IDO_term_list_2009.07.24.xls"))
  (let* ((xls (new 'hssf (namestring (truename file))))
	 (workbook (get-java-field xls "hssfworkbook" t))
	 (sheets (loop for n below (#"getNumberOfSheets" wb)
		      collect (list (#"getSheetName" wb n) (#"getSheetAt" wb n)))))
    (check-isas 
     (loop for sheet in sheets
	append
	  (get-sheet (second sheet))))))

(defun get-sheet (sheet)
  (destructuring-bind (headers . rows)
      (loop for rowno below (#"getPhysicalNumberOfRows" sheet)
	 for row = (#"getRow" sheet rowno)
	 when row
	 collect
	 (loop for colno below (#"getPhysicalNumberOfCells" row)
	    for cell = (#"getCell" row colno)
	    collect (and cell (#"toString" cell))))
    (let ((headerkeys (mapcar (lambda(s)(intern (string-upcase s)'keyword)) headers)))
      (loop for row in rows
	   collect
	   (loop for key in headerkeys
		for cell in row
		collect (list key cell))))))

(defun check-isas (entries)
  (let ((terms 
	 (loop for entry in entries
	    for term = (second (assoc :term entry))
	    for isa = (second (assoc :is_a entry))
	    when (and entry (not (equal term "")))
	    collect (list term isa))))
    (remove-duplicates (set-difference (mapcar 'second terms) (mapcar 'first terms) :test 'equalp) :test 'equalp)))
