package owl2;

import java.io.File;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.util.AutoIRIMapper;

public class ExtractAnnotProp {
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2012-03-29/";
		
		// Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	
    	
    	// load ontology
    	OWLOntology ont = OntologyManipulator.load(path + "merged/cleaned_merged_obi.owl", manager);
 //   	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "merged/"), false);
 //      	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/"), false);
 //      	AutoIRIMapper mapper3 = new AutoIRIMapper(new File(path + "external/iao/"), false);
 //      	manager.addIRIMapper(mapper);
 //   	manager.addIRIMapper(mapper2);
 //   	manager.addIRIMapper(mapper3);
    	
    	// Create factory to obtain a reference to a class
        OWLDataFactory df = manager.getOWLDataFactory();   	 
        //Set the annotation properties
    	OWLAnnotationProperty annotProp = df.getOWLAnnotationProperty(IRI.create("http://purl.obolibrary.org/obo/OBI_9991118"));
       	OWLAnnotationProperty subsetProp = df.getOWLAnnotationProperty(IRI.create("http://www.geneontology.org/formats/oboInOwl#inSubset"));
       	
       	String inSubsetVal = "IEDB";

    	// Create a new ontology that holds application layer annotation property
    	OWLOntology annotOnt = OntologyManipulator.create(manager, "http://purl.obolibrary.org/obo/IEDB_annot.owl");
    	OWLOntology subsetOnt = OntologyManipulator.create(manager, "http://purl.obolibrary.org/obo/IEDB_inSubset.owl");
      	
    	// go through the ontology, find IEDB property, delete the axioms from original ontology and add them in the new ontology
        for (OWLClass cls : ont.getClassesInSignature()) {
            for (OWLAnnotation annotation : cls.getAnnotations(ont, annotProp)) {
            	// get community view label
            	if (annotation.getValue() instanceof OWLLiteral) {
            		OWLLiteral val = (OWLLiteral) annotation.getValue();                        
                    OWLAnnotation annotLabel = df.getOWLAnnotation(annotProp, val);
        		    OWLAxiom ax = df.getOWLAnnotationAssertionAxiom(cls.getIRI(), annotLabel);

        		    // remove axiom from the original ontology
        		    manager.applyChange(new RemoveAxiom(ont, ax));
        		    
        		    // add axioms in the inSubset ontology
                    OWLAnnotation subsetAnnot = df.getOWLAnnotation(subsetProp, df.getOWLLiteral(inSubsetVal));
        		    OWLAxiom subAx = df.getOWLAnnotationAssertionAxiom(cls.getIRI(), subsetAnnot);
        		    manager.applyChange(new AddAxiom(subsetOnt, subAx));
        		    
        		    // add axiom in the user preferred label ontology if value is not empty
        		    if (val.getLiteral().length() > 0) {
        		    	manager.applyChange(new AddAxiom(annotOnt, ax));
        		    } else {
        		    	System.out.println(cls.getIRI().toString() + " " + OBIentity.getLabel(cls,ont,df));
        		    }
            	}
            } 
        } 
        
		OntologyManipulator.saveToFile(manager, ont, path + "merged/cleaned_merged_obi_woIEDB.owl");

		OntologyManipulator.saveToFile(manager, annotOnt, path + "merged/IEDB_annot.owl");
		
		OntologyManipulator.saveToFile(manager, subsetOnt, path + "merged/IEDB_inSubset.owl");
		
		OWLOntology viewOnt = OntologyManipulator.mergeToTargetOnt(manager, ont, subsetOnt);
		viewOnt = OntologyManipulator.mergeToTargetOnt(manager, viewOnt, annotOnt);
		OntologyManipulator.saveToFile(manager, viewOnt, path + "merged/obi_IEDBview.owl");		
	}
}
