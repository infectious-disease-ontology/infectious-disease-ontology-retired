;; run this to generate something to paste into external.owl. Input
;; file should be space separate NCBI taxon id, name. The name is
;; ignored. "#" begun lines are also ignored

(defun gen-external-species (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil :eof)
       while (not (eq line :eof))
       for (id name) = (car (all-matches line "(\\S+)\\s+(.*)" 1 2))
       unless (equal id "#") 	 do (format t "<owl:Class rdf:about=\"http://purl.org/obo/owl/NCBITaxon#NCBITaxon_~a\">
    <rdfs:subClassOf rdf:resource=\"http://purl.obolibrary.org/obo/OBI_0100026\"/>
    <OBI_0000283 rdf:resource=\"http://purl.org/obo/owl/NCBITaxon\"/>
  </owl:Class>~%" id))))


; (gen-external-species "~/obi/trunk/src/ontology/spreadsheets/in/array-express-species.txt")

