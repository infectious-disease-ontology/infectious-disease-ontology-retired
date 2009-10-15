(defun get-sheets (&key file only-sheets)
  (let* ((xls (new 'hssf (namestring (truename file))))
	 (workbook (get-java-field xls "hssfworkbook" t))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (contents (loop for sheet in sheets
		      when (or (not only-sheets)
			       (member (car sheet)  only-sheets :test 'equal))
		      append (get-sheet (second sheet) (first sheet)))))
    (setq @@ sheets)
    (setq @ contents)
    ))

(defun get-sheet (sheet sheet-name)
  (destructuring-bind (headers . rows)
      (loop with first = (#"getFirstRowNum" sheet)
	 for rowno from first to (#"getLastRowNum" sheet) 
	 with nocells = (loop for row below (#"getPhysicalNumberOfRows" sheet)
			   maximize (or (and (not (#"getRow" sheet row)) 0)
					(#"getPhysicalNumberOfCells" (#"getRow" sheet row))))
	 for row = (#"getRow" sheet rowno)
	 for potential = (and row 
			      (list sheet-name rowno
				    (loop for colno below nocells
				       for colcount from 1
				       for cell = (#"getCell" row colno)
				       collect (if (and (equal rowno first) 
							(or (null cell)
							    (and (not (null cell))
								 (equal (#"toString" cell) ""))))
						   (format nil "Column-~a" colcount)
						   (and cell (#"toString" cell))))))
	 collect potential)
    (let ((headerkeys (mapcar (lambda(s)
				(intern (substitute #\- #\space (string-upcase s))'keyword)) (third headers))))
      (loop for (sheet rowno row) in rows
	   when rowno
	 collect
	   (append `((:sheet ,sheet) (:row ,(1+ rowno)))
		   (loop for key in headerkeys
		      for cell in row
		      if (and (equal key :synonym) cell)
		      do (if (equal cell "")
			       (setq cell nil)
			       (setq cell (mapcar (lambda(e) (string-trim " " e)) (split-at-regex cell "[;,]"))))
		      collect (list key cell)))))))