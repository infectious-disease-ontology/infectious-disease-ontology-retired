; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)
;; (load "~/lsw/biopax/proto/obo.lisp")

(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defun translate-pathway ()
  (get-sheets "ido:immunology;ido-s4lps-tlr4.xls"))

