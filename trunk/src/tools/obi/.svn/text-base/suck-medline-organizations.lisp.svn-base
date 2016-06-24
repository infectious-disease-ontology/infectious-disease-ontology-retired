(defun suck-medline-organizations ()
  (let ((page (get-url "http://www.nlm.nih.gov/medlineplus/organizations/all_organizations.html")))
    (with-ontology org (:about "http://purl.obolibrary.org/obo/org.owl" :base "http://purl.obolibrary.org/obo/")
	((annotation-property !foaf:homepage (label "home page"))
	 (owl-imports !<http://purl.obolibrary.org/obo/iao/ontology-metadata.owl>)
	 (class !'material_entity'@obi (label (literal "material entity" :|@en|)) :partial)
	 (class !'organization'@obi (label (literal "organization" :|@en|)) :partial !'material_entity'@obi)
	 (loop for (page name partof)
	    in (all-matches page "(?si)<LI><A HREF=\"([^\"]+)[^>]+>([^<]+)</A>\\s*(.*?)(<br />.*?){0,1}</LI>" 1 2 3)
	    for count from 1000
	    collect (individual (make-uri (format nil "http://purl.obolibrary.org/obo/ORG_~7,'0d" count))
		      (type !'organization'@obi)
		      (value !foaf:homepage (individual (make-uri (#"replaceFirst" page "https" "http"))
					      (type !'information content entity'@obi)
					      (label (literal (format nil "Home page of ~a" name) :|@en|))))
		      (annotation !'definition source'@obi (format nil "WEB:http://www.nlm.nih.gov/medlineplus/organizations/ # Medline Organization"))
		      (label (literal name :|@en|)))))
      org)))
		


    
