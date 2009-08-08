;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "ido")
      `(
	("ido-core;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '("src" "ontology" :wild-inferiors))
				    :name :wild
				    :type :wild))
	("tools;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '("src" :wild-inferiors))
				    :name :wild
				    :type :wild))
	("lib;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 2)
						       '( :wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :ido
    :name "Infectious disease ontology"
    :licence "BSD"
    :components
    ((:file "ido-to-owl")
     (:file "obo")
     (:jar-directory "lib" :pathname "ido:lib;java;poi-3-2-FINAL;")
     )
    :depends-on (owl))

;;;; eof
