(defun obsolete-properties (kb)
  (mapcar 'aterm-to-sexp
	  (mapcan 'set-to-list
		  (set-to-list
		   (#"getSubs" (#"getTaxonomy" (#"getRBox" (kb-kb kb)))
			       (get-entity !oboinowl:ObsoleteProperty kb))))))

(defun report-users-of-obsolete (kb)
  (let ((term2class (make-hash-table :test 'equal)))
    (loop for class in (descendants !owl:Thing kb)
       do (loop for term in (get-terms-referenced-by-class class kb)
	     do (pushnew class (gethash term term2class))))
    (loop for obsolete in (union (descendants !oboinowl:ObsoleteClass kb) (obsolete-properties kb))
       do
       (loop for user in (gethash obsolete term2class) 
	  do
	    (unless (equal user obsolete)
	      (format t "~a(~a) uses ~a(~a)~%"
		      (car (rdfs-label user kb))
		      (#"replaceAll" (uri-full user) ".*/" "")
		      (car (rdfs-label obsolete kb))
		      (#"replaceAll" (uri-full obsolete) ".*/" "")
		      ))))))

