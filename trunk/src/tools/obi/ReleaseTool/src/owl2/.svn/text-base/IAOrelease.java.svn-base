package owl2;

import java.io.File;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.RemoveImport;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.AutoIRIMapper;

public class IAOrelease {

	/**
	 * merge imported and obsolete terms with iao-main.owl
	 * @param args
	 */
	public static void main(String[] args) {
        String external = "http://purl.obolibrary.org/obo/iao/dev/external.owl";
        String externalDerived = "http://purl.obolibrary.org/obo/iao/dev/externalDerived.owl";
        String externalByHand = "http://purl.obolibrary.org/obo/iao/dev/externalByHand.owl";
    	
        String reasonerName = "hermit";
        
		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/iao/releases/2012-01-05/";
	    String saveIaoFilename = path + "merged/iao-main-infer.owl";	
		String inferOntURIStr = "http://purl.obolibrary.org/obo/iao/dev/iao_inferredSuperClasses.owl";
	    String saveInferFilename = path + "merged/iao-inferredSuperClasses.owl";	
		
	    // Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	 
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "ontology/"), false);
    	manager.addIRIMapper(mapper);
                         
        OWLOntology iaoOnt = OntologyManipulator.load(path + "ontology/iao-main.owl", manager);
    	//OntologyManipulator.printPrefixNSs(manager, iaoOnt);

    	OWLDataFactory df = manager.getOWLDataFactory();

	    // get all imported ontologies
      	Set<OWLOntology> importOnts = iaoOnt.getImports();

      	// remove imports statements from the loaded ontology
     	for(OWLOntology importOnt: importOnts) {    		
     		IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(external)) || importOntIRI.equals(IRI.create(externalByHand)) || importOntIRI.equals(IRI.create(externalDerived))) { 
    			RemoveImport ri = new RemoveImport(iaoOnt, df.getOWLImportsDeclaration(importOntIRI));
    			manager.applyChange(ri);
    		}
     	}
     	
     	// merge the removed imported ontologies to the loaded one
     	for(OWLOntology importOnt: importOnts) {
     		IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(external)) || importOntIRI.equals(IRI.create(externalByHand)) || importOntIRI.equals(IRI.create(externalDerived))) {
    			// OntologyManipulator.printPrefixNSs(manager, importOnt);
    			iaoOnt = OntologyManipulator.mergeToTargetOnt(manager, iaoOnt, importOnt);
    		}
     	} 
    	
      	// generate the inferred hierarchy and clean the super classes
     	OWLReasoner reasoner = OWLReasonerRunner.runReasoner(manager, iaoOnt, reasonerName);
    	
		iaoOnt = OWLReasonerRunner.getCleanedOntologyWithInferredSuperClasses(manager, iaoOnt, inferOntURIStr, reasoner);
		OntologyManipulator.saveToFile(manager, iaoOnt, saveIaoFilename);
		
		if (manager.contains(IRI.create(inferOntURIStr))) {
			OWLOntology inferOnt = manager.getOntology(IRI.create(inferOntURIStr));
			OntologyManipulator.saveToFile(manager, inferOnt, saveInferFilename);
		}
	}
}
