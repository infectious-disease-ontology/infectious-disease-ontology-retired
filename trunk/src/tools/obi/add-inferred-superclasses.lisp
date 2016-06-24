'(defun write-inferred-superclasses (&optional (kb (load-kb-jena "obi:newids;obidi.owl"))
				    (path "obi:newids;superclasses.owl"))
  (if (check kb)
      (with-ontology inferred (:base (format nil "http://purl.obolibrary.org/obo/obi/~a.~a"
					     (pathname-name (translate-logical-pathname path))
					     (pathname-type (translate-logical-pathname path)))) 
	  ((loop for class in (descendants !owl:Thing kb)
	      append
	      (loop for super in (parents class kb)
		 collect (class class :partial super))))
	(check inferred)
	(write-rdfxml inferred path))
      (error "KB isn't consistent, so inferred superclasses won't be correct")))
				       
(defun write-inferred-superclasses (&optional (kb (load-kb-jena "obi:newids;obidi.owl"))
				      (path "obi:newids;superclasses.owl"))
  (if (check kb)
      (with-ontology inferred (:base (format nil "http://purl.obolibrary.org/obo/obi/~a.~a"
					     (pathname-name (translate-logical-pathname path))
					     (pathname-type (translate-logical-pathname path)))) 
	  ((loop for (sub super) in
		(set-difference
		 (loop for class in (descendants !owl:Thing kb)
		    append
		    (loop for super in (parents class kb)
		       collect (list class super)))
		 (sparql '(:select (?sub ?super) (:distinct t)
			   (?sub !rdfs:subClassOf ?super))
			 :use-reasoner :none :kb kb) :test 'equal)
		collect
		(class sub :partial super)))
					;	(check inferred)
	(write-rdfxml inferred path))
      (error "KB isn't consistent, so inferred superclasses won't be correct")))
				       
(defun write-stated-superclasses (&optional (kb (load-kb-jena "obi:newids;obidi.owl"))
				    (path "obi:newids;stated-superclasses.owl"))
  (with-ontology inferred (:base (format nil "http://purl.obolibrary.org/obo/obi/~a.~a"
					 (pathname-name (translate-logical-pathname path))
					 (pathname-type (translate-logical-pathname path)))) 
      ((loop for (sub super) in
	    (sparql '(:select (?sub ?super) (:distinct t)
		      (?sub !rdfs:subClassOf ?super)
		      (:filter (and (not (isblank ?sub))
				(not (equal ?sub ?super))
				(not (isblank ?super))
				(not (equal ?sub !owl:Nothing))
				(not (equal ?sub !owl:Thing))
				(not (equal ?super !owl:Nothing))
				(not (equal ?super !owl:Thing)))))
		    :use-reasoner :none :kb kb)
	    collect (class sub :partial super)))
    (write-rdfxml inferred path)))