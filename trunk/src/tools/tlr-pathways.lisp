(defvar *ido-spreadsheets*
  '(("ido-s4lps-tlr4.xlsx" "LPS recognition and signaling via TLR4")
    ("ido-tlr2.xlsx" "TLR2 Lta signaling")
    ("TLR3.xlsx" "dsRNA recognition and signaling via TLR3")))

(defun process-spreadsheets ()
  (loop for (sheet name) in *ido-spreadsheets*
       for book = (make-instance 'ido-pathway-book
				 :book-path (merge-pathnames sheet "ido:immunology;")
				 :pathway-name name)
     do
       (parse-book book)
       (write-external.owl Book)
       (write-pathway.owl Book)
       (write-external-derived.owl book)
       collect book))

(defun check-complex-handles (books)
  (loop for book in books do (print-db book)
       (loop for block in (blocks-of-type book 'parsed-complex-block)
	  do (loop for row in (parsed-rows block) do (format t "~a - ~a:~a ~{~a:~a~^ ~}~%"
							     row
							     (handle row) (if (lookup-handle (in-sheet block) (handle row)) "<ok>" "<nohandle>")
							     (loop for h in (complex-elements row) collect h 
								collect (if (lookup-handle (in-sheet block) h) "<ok>" "<nohandle>")))))))


;(create-external-derived :kb (load-ontology "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/proto/TLR2-Lta-signaling-pathway-external.owl") :output-path "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/proto/TLR2-Lta-signaling-pathway-external-derived.owl")

