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
import org.semanticweb.owlapi.util.AutoIRIMapper;

// TODO
// need be able to add editor preferred label to all types of terms, such as object property

// write how many terms changed in the future
public class AddEditorPreferredTerms {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// locations of ontology files
 		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/trunk/src/ontology/";
 		String ontFile = path + "branches/obi.owl";	
		String newOntFilename = path + "branches/obi_new.owl";		
		
	   	String editorTermAnnotProp = "http://purl.obolibrary.org/obo/IAO_0000111";
	   	
 		// Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	 
        
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "branches/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/iao-bfo2/"), false);
       	manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
    	
    	// load ontology
    	OWLOntology ont = OntologyManipulator.loadFromFile(ontFile, manager);
    	
        // Create factory to obtain a reference to a class
        OWLDataFactory df = manager.getOWLDataFactory();   	     
        
        //Set the annotation properties
        OWLAnnotationProperty preferredProp = df.getOWLAnnotationProperty(IRI.create(editorTermAnnotProp));
 
    	int count = 0;
    	// go through the ontology, add editor preferred term if there is no this property
        for (OWLClass cls : ont.getClassesInSignature()) {
        	boolean hasPreferredTerm = false;
         	
        	String id = cls.getIRI().toString();
        	if (!id.contains("http://purl.obolibrary.org/obo/OBI_")) continue;
        	
        	if(!cls.getAnnotations(ont, preferredProp).isEmpty()) {
            	hasPreferredTerm = true;
            }
        	
            // set editor preferred term as the rdfs:label, we can omit the first checking, set all editor preferred label as rdfs:label
            if (!hasPreferredTerm) {   
            	OWLLiteral labelOWLLiteral = OBIentity.getLabelOWLLiteral(cls, ont, df);
            	if (labelOWLLiteral != null) {
            		OWLAnnotation preferredLabel = df.getOWLAnnotation(preferredProp, labelOWLLiteral);
            		OWLAxiom ax = df.getOWLAnnotationAssertionAxiom(cls.getIRI(), preferredLabel);
            		manager.applyChange(new AddAxiom(ont, ax));
            		count ++;
            	}
            }
        }    
        
        System.out.println("Editor preferred term annotation was added to a total of " + count + " OBI classes.");
        
		OntologyManipulator.saveToFile(manager, ont, newOntFilename);
	}
}
