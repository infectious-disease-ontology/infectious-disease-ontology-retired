(defun write-purls (kb kbprev &optional (dest "obi:build;list-purls.xml"))
  (let((kb-purls
	(sparql
	 '(:select (?thing) (:distinct t)
	   (:union
	    ((?thing ?p ?o))
	    ((?s ?thing ?o))
	    ((?s ?p ?thing)))
	   (:filter (and (isuri ?thing) (regex (str ?thing) "OBI_\\d+")))
	   )
	 :kb kb :use-reasoner :none :flatten t))
       (kbprev-purls
	(and kbprev
	     (sparql
	      '(:select (?thing) (:distinct t)
		(:union
		 ((?thing ?p ?o))
		 ((?s ?thing ?o))
		 ((?s ?p ?thing)))
		(:filter (and (isuri ?thing) (regex (str ?thing) "OBI_\\d+")))
		)
	      :kb kbprev :use-reasoner :none :flatten t))))
    (flet ((doit (out)

	     (format out "<recs>~%")
	     (loop for new in (set-difference kb-purls kbprev-purls)
		for purl = (#"replaceFirst" (uri-full new) ".*/obo/" "/obo/")
		for id = (#"replaceFirst" purl ".*/" "")
		do (format out "<rec><purl>~a</purl><url>http://sw.neurocommons.org/obiterm/~a</url><id>ALANRUTTENBERG</id><id>OBI</id><type>User_Batch_Add</type></rec>~%" purl id ))
	     (format out "</recs>~%")
	     ))
      (if (or (eq dest t) (streamp dest))
	  (doit dest)
	  (with-open-file (out dest
			       :direction :output
			       :if-exists :supersede)
	    (doit out))
	  )
      (when (set-difference kbprev-purls kb-purls)
	(format t "Hmm, we seem to have lost some ids (deprecation lossage?): ~%~{~a~%~}"
		(set-difference kbprev-purls kb-purls)))    
      )))



;; e.g. (create-dated-purls "2007-06-27" "alanruttenberg" "alanspassword")
;1. /obo/2008-06-27/obi.owl http://obi.svn.sourceforge.net/svnroot/obi/releases/2008-06-27/merged/OBI.owl
;2. /obo/2008-06-27/obi/branches/ http://obi.svn.sourceforge.net/svnroot/obi/releases/2008-06-27/branches/

(defparameter *obi-release-dir* "http://obi.svn.sourceforge.net/svnroot/obi/releases/")

(defun create-dated-purls (datestring user password)
  (list
   (create-new-purl (format nil "/obo/~a/obi.owl" datestring)
		    (format nil "~a~a/merged/OBI.owl"  *obi-release-dir* datestring) user password '("obi"))
   (create-new-purl (format nil "/obo/~a/obi/branches/" datestring)
		    (format nil "~a~a/branches/" *obi-release-dir* datestring) user password '("obi") t)))

;; e.g. (update-current-purls "2007-06-27" "alanruttenberg" "alanspassword")
;1. /obo/obi.owl http://obi.svn.sourceforge.net/svnroot/obi/releases/2008-06-27/merged/OBI.owl
;2. /obo/obi/protege/obi.owl http://obi.svn.sourceforge.net/svnroot/obi/releases/2008-06-27/merged/protege/OBI-ProtegeFriendly.owl

(defun update-current-purls (datestring user password)
  (list
   (update-purl "/obo/obi.owl" 
		(format nil "http://obi.svn.sourceforge.net/svnroot/obi/releases/~a/merged/OBI.owl" datestring)
		user password '("obi") (format nil "release of ~a" datestring))
   (update-purl  "/obo/obi/protege/obi.owl"
		 (format nil "http://obi.svn.sourceforge.net/svnroot/obi/releases/~a/merged/protege/OBI-ProtegeFriendly.owl" datestring)
		 user password '("obi") (format nil "release of ~a" datestring))
   (update-purl  "/obo/obi/protege/obi.pprj"
		 (format nil "http://obi.svn.sourceforge.net/svnroot/obi/releases/~a/merged/protege/OBI-ProtegeFriendly.pprj" datestring)
		 user password '("obi") (format nil "release of ~a" datestring))
   (update-purl  "/obo/obi/protege/"
		 (format nil "http://obi.svn.sourceforge.net/svnroot/obi/releases/~a/merged/protege/" datestring)
		 user password '("obi") (format nil "release of ~a" datestring))
   (update-purl  "/obo/obi/report.html"
		 (format nil "http://obi.svn.sourceforge.net/svnroot/obi/releases/~a/obi-lsw-report.html" datestring)
		 user password '("obi") (format nil "release of ~a" datestring))))

(defparameter *obi-purls* 
  (list
   "/obo/obi.owl"
   "/obo/obi/protege/obi.owl"
   "/obo/obi/protege/obi.pprj"
   "/obo/obi/protege/"
   "/obo/obi/report.html"
   "/obo/obi/doc/"
   "/obo/obi/owldoc"
   "/obo/obi/repository/"
   "/obo/obi/tracker"
   "/obo/obi/wiki/"
   "/obo/obi/calendar"))

(defun report-current-purls ()
  (format t "$svn = \"http://obi.svn.sourceforge.net/svnroot/obi\"~%")
  (loop for apurl in *obi-purls* 
     for (url purl partial . maintainers) = (get-purl apurl)
     for retrieved = (get-url purl :persist nil :ignore-errors t)
     do (format t "~a: ~a~a~a~a~%" (subseq url 21)
		(and purl (#"replaceFirst" purl "http://obi.svn.sourceforge.net/svnroot/obi" "\\$svn"))
		(if partial " (partial redirect)" "")
		(if (member "OBI" maintainers :test 'equalp) "" " Missing \"obi\" as maintainer!")
		(if retrieved "" " !! Failure fetching !!" )
		)))
	     

