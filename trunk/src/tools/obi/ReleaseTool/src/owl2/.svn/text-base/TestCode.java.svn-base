package owl2;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.RDFOntologyFormat;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OWLOntologyWalker;
import org.semanticweb.owlapi.util.OWLOntologyWalkerVisitor;
import org.semanticweb.owlapi.vocab.PrefixOWLOntologyFormat;

import java.util.Collections;

public class TestCode {
    public static void main(String[] args) {
		String targetFilename = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/trunk/src/ontology/branches/obi.owl";
		String saveFilename = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/trunk/src/ontology/branches/merged_obi2.owl";		
		
		// Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	     
    	
    	// load ontology
     	OWLOntology targetOnt = OntologyManipulator.loadFromFile(targetFilename, manager);
    	
     	try {
     		OWLOntologyFormat ontFormat = manager.getOntologyFormat(targetOnt);
     		if (ontFormat.isPrefixOWLOntologyFormat()) {
     			 PrefixOWLOntologyFormat prefixOntFormat = ontFormat.asPrefixOWLOntologyFormat(); 
     		}
     	} catch (UnknownOWLOntologyException e) {
     		e.printStackTrace(); 		
     	}
     	
    	// targetOnt = OntologyManipulator.setOntologyID(manager, targetOnt, "http://purl.obolibrary.org/obo/obi_merged.owl");
    }
}
