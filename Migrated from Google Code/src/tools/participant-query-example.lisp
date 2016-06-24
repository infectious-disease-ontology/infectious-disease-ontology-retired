;; Read http://clarkparsia.com/weblog/2007/12/20/how-distinguished-is-your-variable/

;; This is the beginnings of some tests to see whether sparql dl can
;; answer queries against the "some" side of "all some" statements.

#|
The case to consider is where we've represented a reaction R
subClassof has_participant some A.  We would like to query what is the
type of the participants of R.

To get the explicitly asserted participant types, we can query the
axioms. However, if there are other assertions that might further
specify the type of the participant, we can't be sure to get those if
we only look at the axioms.

The strategy here is to use a hybrid. Get the base types of the
participants by querying the axioms. Then, for each type, construct a
query that tells you the rest of the types. For this we
use "undistinguished variables" in sparql(dl). If you use a bnode in a
query, then pellet will bind that bnode to an inferred individuals
instead of only an explicit individual in the ontology.

So we say:

Suppose I had an instance of reaction - call it r1
Suppose that r1 has_participant some other instance, call it p1.
Suppose p1 was an instance of A.
What other types would p1 be?

As of Pellet 2.2.0 this doesn't work, and in fact can give completely
incorrect answers in some cases. We'll see whether a repaired pellet
can do this.

See test-reaction-cyto below for examples.

|#
;;

;; Here is the test case

(defun test-reaction-cyto ()
  (with-ontology simple (:about !chainont :collecting t)
    ((asq
      (declaration (class !reaction)) ;reaction
      (declaration (class !s1)) ; substrate 1
      (declaration (class !s2)) ; substrate 2
      (declaration (class !p1)) ; product 1
      (declaration (class !cyto)) ; cytoplasm
      (declaration (object-property !has_participant))
      (annotation-assertion !rdfs:label !has_participant "has_participant")
      (declaration (object-property !located_in)) ; relating a continuant to a place
      (declaration (object-property !occurs_in)) ; relating a process to a place

      ;; the chain says: If a process occurs in a place, then all it's
      ;; participants are located in that place. But it's better read
      ;; as: Suppose I start at some instance (i) and follow backwards
      ;; the has_participant property (leading to a process) and then
      ;; forwards on the occurs_in property (leading to a place). Then
      ;; assert that i (the instance I started with) is located_at
      ;; that place.

      (sub-object-property-of (object-property-chain
			       (object-inverse-of !has_participant) !occurs_in)
			   !located_in)

      ;; all classes are disjoint
      (disjoint-classes !reaction !s1 !s2 !p1 !cyto)

      ;; note using an intersection of the restrictions defining
      ;; !reaction make it more annoying to query axioms

      ;; the reaction is defined as s1 + s2 -> p1
      (sub-class-of !reaction (object-some-values-from !has_participant !s1))
      (annotation-assertion !rdfs:label !s1 "s1")
      (sub-class-of !reaction (object-some-values-from !has_participant !s2))
      (sub-class-of !reaction (object-some-values-from !has_participant !p1))
      
      ;; There are only three participants 
      (sub-class-of !reaction (object-exact-cardinality 3 !has_participant))

      ;; Now define a subclass of reaction that is located in the cyto
      (sub-class-of !reaction_cyto !reaction)
      (annotation-assertion !rdfs:label !reaction_cyto "reaction_cyto")
      (sub-class-of !reaction_cyto (object-some-values-from !occurs_in !cyto))

      ;; Now define a subclass of s1 that is located in the cyto. We
      ;; will use this to test our query. If we ask for what the other
      ;; types of the participant of reaction_cyto that is of type s1,
      ;; we should get s1_cyto back.

      (equivalent-classes !s1_cyto (object-intersection-of !s1 (object-some-values-from !located_in !cyto)))
      ))

    ;; run reasoner
    (check-ontology simple :reasoner :pellet-sparql )

    ;; First query. We hope that one of the bindings of ?type will be s1_cyto. Not in pellet 2.2.0 :(
    (sparql '(:select (?type) ()
	      (:_r !rdf:type !reaction_cyto)
	      (:_r !has_participant :_s1)
	      (:_s1 !rdf:type !s1)
	      (:_s1 !rdf:type ?type))
	    :kb simple :use-reasoner :sparqldl
	    :trace "hope that ?type is s1_cyto")

    ;; Second query. We hope that the binding of ?loc will be cyto. Not in pellet 2.2.0 :(
    (sparql '(:select (?loc_type) ()
	      (:_r !rdf:type !reaction_cyto)
	      (:_r !has_participant :_s1)
	      (:_s1 !rdf:type !s1)
	      (:_s1 !located_in :_loc)
	      (:_loc !rdf:type ?loc_type))
	    :kb simple :use-reasoner :sparqldl
	    :trace "hope that ?loc is cyto")

    ;; third query - would be nice if Pellet supported this. 
    ;; not supported yet
;    (sparql '(:select (?type) ()
;	      (!reaction_cyto !rdfs:subClassOf (!ex:has_participant :some ?type)))
;	    :kb simple :use-reasoner :sparqldl :syntax :terp
;	    :trace t)

    ;; Query on the axioms to find the explicit participants of !reaction_cyto
    (sparql '(:select (?type) ()
	      (!reaction_cyto !rdfs:subClassOf ?c)
	      (?c !rdfs:subClassOf :_r)
	      (:_r !owl:onProperty !has_participant)
	      (:_r !owl:someValuesFrom ?type))
	    :kb simple :use-reasoner :none ;; note doesn't give answers when using sparql-dl
	    :trace "Ask via axioms what participants in reaction_cyto is")
    
    ;; sanity check - verify that the inference that we want happens
    ;; by seeing what classes s1s that participate in reaction_cyto
    ;; are subsumed by. If the inference is made the answer would be
    ;; s1_cyto. If not then s1.

    (format t "Sanity check - is the inference we want happening? If s1_cyto is returned then yes.")
    (print-db (parents "s1 and inverse(has_participant) some reaction_cyto" simple))

;; (sparql '(:select (?type) ()
;; 	      (!ex:s1 :and !ex:participates_in :some !reaction_cyto) !rdfs:subClassOf ?type)
;; 	    :kb simple :use-reasoner :sparqldl :syntax :terp
;; 	    :trace t)
    simple))

#|
Here is the output of the above:

(test-reaction-cyto)

Query: hope that ?type is s1_cyto
PREFIX ex: <http://example.com/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?type
WHERE { 
_:R rdf:type ex:reaction_cyto . 
_:R ex:has_participant _:S1 . 
_:S1 rdf:type ex:s1 . 
_:S1 rdf:type ?type . } 
Results:

Query: hope that ?loc is cyto
PREFIX ex: <http://example.com/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?loc_type
WHERE { 
_:R rdf:type ex:reaction_cyto . 
_:R ex:has_participant _:S1 . 
_:S1 rdf:type ex:s1 . 
_:S1 ex:located_in _:LOC . 
_:LOC rdf:type ?loc_type . } 
Results:

Query: Ask via axioms what participants in reaction_cyto is
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ex: <http://example.com/>
SELECT ?type
WHERE { 
ex:reaction_cyto rdfs:subClassOf ?c . 
?c rdfs:subClassOf _:R . 
_:R owl:onProperty ex:has_participant . 
_:R owl:someValuesFrom ?type . } 
Results:
!ex:s2
!ex:p1
!ex:s1

Sanity check - is the inference we want happening? If s1_cyto is returned then yes.
(parents "s1 and inverse(has_participant) some reaction_cyto" simple)     33%

|#


#----------------------------------------------------------------

;; This case is a sanity check. It uncovered a bug in Pellet 2.2.0 so I keep it here. 

;; Here's the command line test: 
;;  sh pellet.sh query -q file:///Users/alanr/Downloads/2010-07-19/pellet-2.2.0/undistinguished.sparql --bnode file:///Users/alanr/Desktop/simple.owl
;; Query Results (1 answers): 
;; type
;; ====
;; r1  

(defun test-sparql-bug ()
  (with-ontology simple (:collecting t)
    ((asq
      (declaration (class !reaction))
      (declaration (class !s1))
      (declaration (class !s2))
      (declaration (class !p1))
      (declaration (object-property !has_participant))
      (disjoint-classes !reaction !s1 !s2 !p1)
      (sub-class-of !reaction
		    (object-intersection-of
		     (object-some-values-from !has_participant !s1)
		     (object-some-values-from !has_participant !s2)
		     (object-some-values-from !has_participant !p1)
		     (object-exact-cardinality 3 !has_participant)))
      (declaration (namedindividual !r1))
      (class-assertion !reaction !r1)))
    (check-ontology simple :reasoner :pellet-sparql )
    (sparql '(:select (?type) ()
	      (:_r !rdf:type !reaction)
	      (:_r !has_participant :_s1)
	      (:_s1 !rdf:type !s1)
	      (:_s1 !rdf:type ?type))
	    :kb simple :use-reasoner :sparqldl
	    :trace t)
    (sparql '(:select (?type) ()
	      (!r1 !has_participant :_s1)
	      (:_s1 !rdf:type !s1)
	      (:_s1 !rdf:type ?type))
	    :kb simple :use-reasoner :sparqldl
	    :trace t)
    simple))