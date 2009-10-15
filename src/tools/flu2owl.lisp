;; another obo to owl converter

;; (load "~/lsw/biopax/proto/obo.lisp")
;; (setq obo (make-instance 'obo :path "/Users/alanr/Downloads/2009-07-16/influenzO.obo.txt"))
;; (read-obo obo)
;; (flu-to-owl) -> ~/Desktop/flu.owl

(defun getf-multiple (plist indicator)
  (loop for (key value) on plist by #'cddr when (eq indicator key) collect value))

(defun flu-to-owl ()
  (flet ((uri-ify (id)
	   (cond ((#"matches" id "^FLU:.*")
		  (make-uri (#"replaceFirst" id "FLU:" "http://purl.obolibrary.org/obo/FLU_")))
		 ((#"matches" id "^ID:.*")
		  (make-uri (#"replaceFirst" id "ID:" "http://purl.obolibrary.org/obo/IDO_")))
		 ((#"matches" id "^OBO_REL:.*")
		  (make-uri (#"replaceFirst" id "^OBO_REL:" "http://www.obofoundry.org/ro/ro.owl#")))
		 (t (error "don't know how to convert id ~a to uri" id)))))
    (with-ontology flu ()
	((loop for record in (terms obo) ;repeat 5 
	    for name = (getf (cdr record) :name)
	    for id = (getf (cdr record) :id)
	    for is_a = (getf-multiple (cdr record) :is_a)
	    for xrefs = (getf-multiple (cdr record) :xref)
	    for def = (getf (cdr record) :def)
	    collect (apply 'class  (label name) (uri-ify id)
			   (if (car def) (annotation !obo:IAO_0000115 (literal (car def)  :|@en|)))
			   (if (third def) (annotation !obo:IAO_0000119  (third def)))
			   (append
			    (if (car xrefs)
				(loop for xref in xrefs collect
				    (annotation !oboinowl:hasDbXref (format nil "~a" xref))))
			    (list :partial)
			     (if (getf (cdr record) :is_obsolete)
			       (list !oboinowl:ObsoleteClass)
			       (mapcar #'uri-ify is_a))))))
;      (prin1 (rdfxml flu))
      (write-rdfxml flu))))