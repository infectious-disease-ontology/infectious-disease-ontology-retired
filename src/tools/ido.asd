;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)


(setf (logical-pathname-translations "ido")
      `(
	("ido-core;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '("src" "ontology" "ido-core" :wild-inferiors))
				    :name :wild
				    :type :wild))
	("immunology;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '("src" "ontology" "immunology" :wild-inferiors))
				    :name :wild
				    :type :wild))
	("tools;**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						       '(:wild-inferiors))
				    :name :wild
				    :type :wild))
	("lib;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '("lib" :wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :ido
    :name "Infectious disease ontology"
    :licence "BSD"
    :components
    ((:file "ido-to-owl")
     (:file "pathway-to-owl")
     (:file "obo")
     (:module "obi"
	      :components
	      ((:file "create-external-derived")))
     )
    :depends-on (owlapi3 read-ms-docs))

;;;; eof
