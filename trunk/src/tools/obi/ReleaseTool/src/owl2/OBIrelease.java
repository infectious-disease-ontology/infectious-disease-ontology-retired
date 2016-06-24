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

public class OBIrelease {
	
	/* SVN check out
	 * 
	 * get SVN log
	 * 
	 * assign new IDs
	 * 
	 * quality check
	 * - redundant rdfs:label
	 * - curation status, any missing, any in uncurated status, any incorrect status, missing required annotations for stated status
	 * - inconsistent rdfs:label with editor preferred labels
	 * - check any terms without rdfs:label with given IRI pattern (not include BFO) 
	 * 
	 * 
	 * 
	 * add disjoint axioms
	 * 
	 * 
	 * 
	 * reasoning and remove duplicated upper classes
	 * 
	 * merge all imported terms except IAO and BFO
	 * 
	 * remove branches, _defined_material, _defined_process, 
	 */
	public static void main(String[] args) {
		
		// mergeExternals ();
		
		// inferSuperClasses ();

		mergeDisjoints();

		mergeIAO ();
     	
     	/*
		// merge obi with merged iao.owl
		String targetFilename = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2011-12-13/merged/Copy of cleaned_merged_obi.owl";
  		String saveFilename = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2011-12-13/merged/merged_obi_iao2.owl";		
		
    	// load ontology
     	OWLOntology targetOnt = OntologyManipulator.loadFromFile(targetFilename, manager);
      	targetOnt = OntologyManipulator.mergeToTargetOnt(manager, targetOnt, iaoOnt);
		
     	// targetOnt = OntologyManipulator.setOntologyID(manager, targetOnt, "http://purl.obolibrary.org/obo/obi_merged.owl");
    	OntologyManipulator.saveToFile(manager, targetOnt, saveFilename);
    	*/		
	}

	static void mergeDisjoints () {
		System.out.println("--- merge disjoints ---");
		
 		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2012-03-29/";
 		String ontFilename = path + "merged/obi_merged_inferred.owl";
		String disjointFilename = path + "branches/disjoints.owl";
	    String saveFilename = path + "merged/obi_disjoints.owl";	
		
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	 
        
        // load imported ontologies locally
    	// AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "branches/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/"), false);
       	AutoIRIMapper mapper3 = new AutoIRIMapper(new File(path + "external/iao/"), false);
       	// manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
    	manager.addIRIMapper(mapper3);
                         
    	// load obi ontology
        OWLOntology ont = OntologyManipulator.load(ontFilename, manager);
        // check prefix defined in the ontology and where the ontology loaded from
    	//OntologyManipulator.printPrefixNSs(manager, ont);

        // load disjoint ontology
     	OWLOntology disjointOnt = OntologyManipulator.loadFromFile(disjointFilename, manager);
     	// merge disjoint.owl to the obi.owl
     	ont = OntologyManipulator.mergeToTargetOnt(manager, ont, disjointOnt);
     	OntologyManipulator.saveToFile(manager, ont, saveFilename);
     	System.gc();
	}
	
	static void mergeExternals () {
		System.out.println("--- merge external ---");

		String external = "http://purl.obolibrary.org/obo/obi/external.owl";
        String externalDerived = "http://purl.obolibrary.org/obo/obi/externalDerived.owl";
        String externalByHand = "http://purl.obolibrary.org/obo/obi/external-byhand.owl";
 
		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2012-03-29/";
 		String ontFilename = path + "merged/obi.owl";
	    String saveFilename = path + "merged/obi_merged.owl";	

        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        
        // load imported ontologies locally
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "branches/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/"), false);
       	AutoIRIMapper mapper3 = new AutoIRIMapper(new File(path + "external/iao/"), false);
       	manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
    	manager.addIRIMapper(mapper3);
                         
    	// load obi_disjoints ontology
        OWLOntology ont = OntologyManipulator.load(ontFilename, manager);
        // check prefix defined in the ontology and where the ontology loaded from
        //OntologyManipulator.printPrefixNSs(manager, ont);

    	OWLDataFactory df = manager.getOWLDataFactory();

	    // get all imported ontologies
      	Set<OWLOntology> importOnts = ont.getImports();

      	// remove imports statements from the loaded ontology
     	for(OWLOntology importOnt: importOnts) {    		
			// OntologyManipulator.printPrefixNSs(manager, importOnt);
			IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(external)) || importOntIRI.equals(IRI.create(externalByHand)) || importOntIRI.equals(IRI.create(externalDerived))) { 
    			RemoveImport ri = new RemoveImport(ont, df.getOWLImportsDeclaration(importOntIRI));
    			manager.applyChange(ri);
    		}
     	}
    	
     	// merge the removed imported ontologies to the loaded one
     	for(OWLOntology importOnt: importOnts) {
     		IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(external)) || importOntIRI.equals(IRI.create(externalByHand)) || importOntIRI.equals(IRI.create(externalDerived))) {
    			// OntologyManipulator.printPrefixNSs(manager, importOnt);
    			ont = OntologyManipulator.mergeToTargetOnt(manager, ont, importOnt);
    		}
     	} 
     	
     	OntologyManipulator.saveToFile(manager, ont, saveFilename);

    	System.gc();
	}
	
	static void inferSuperClasses () {
		System.out.println("--- reasoning on merged file ---");

		String reasonerName = "hermit";

		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2012-03-29/";
		String ontFilename = path + "merged/obi_merged.owl";
	    String saveFilename = path + "merged/obi_merged_inferred.owl";	
		String inferOntURIStr = "http://purl.obolibrary.org/obo/obi/obi_inferredSuperClasses.owl";
	    String saveInferFilename = path + "branches/obi_inferredSuperClasses.owl";	
		
	    // Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        
        // load imported ontologies locally
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "branches/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/"), false);
       	AutoIRIMapper mapper3 = new AutoIRIMapper(new File(path + "external/iao/"), false);
       	manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
    	manager.addIRIMapper(mapper3);
                         
    	// load merged obi ontology
        OWLOntology ont = OntologyManipulator.load(ontFilename, manager);
    	//OntologyManipulator.printPrefixNSs(manager, ont);
    	
      	// generate the inferred hierarchy and clean the super classes
     	OWLReasoner reasoner = OWLReasonerRunner.runReasoner(manager, ont, reasonerName);
    	
		ont = OWLReasonerRunner.getCleanedOntologyWithInferredSuperClasses(manager, ont, inferOntURIStr, reasoner);
		OntologyManipulator.saveToFile(manager, ont, saveFilename);
		
     	//ont = OWLReasonerRunner.getOntologyWithInferredSuperClasses(manager, ont, inferOntURIStr, reasoner);
     	//OntologyManipulator.saveToFile(manager, ont, saveFilename);
     	
		if (manager.contains(IRI.create(inferOntURIStr))) {
			OWLOntology inferOnt = manager.getOntology(IRI.create(inferOntURIStr));
			OntologyManipulator.saveToFile(manager, inferOnt, saveInferFilename);
		}
 	}
	
	static void mergeIAO () {
		System.out.println("--- merge IAO BFO 2.0 version ---");

        String iao = "http://purl.obolibrary.org/obo/iao/dev/iao.owl";
		String iao_main = "http://purl.obolibrary.org/obo/iao/dev/iao-main.owl";
        String ont_meta = "http://purl.obolibrary.org/obo/iao/dev/ontology-metadata.owl";
 
		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2012-03-29/";
 		String ontFilename = path + "merged/obi_disjoints.owl";
	    String saveFilename = path + "merged/obi_merged_iao.owl";	

        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        
        // load imported ontologies locally
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "merged/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "merged/iao-merged/"), false);
       	manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
                         
    	// load obi_disjoints ontology
        OWLOntology ont = OntologyManipulator.load(ontFilename, manager);
        // check prefix defined in the ontology and where the ontology loaded from
        // OntologyManipulator.printPrefixNSs(manager, ont);

    	OWLDataFactory df = manager.getOWLDataFactory();

	    // get all imported ontologies
      	Set<OWLOntology> importOnts = ont.getImports();

      	// remove imports statements from the loaded ontology
     	for(OWLOntology importOnt: importOnts) {    		
			// OntologyManipulator.printPrefixNSs(manager, importOnt);
			IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(iao)) || importOntIRI.equals(IRI.create(iao_main)) || importOntIRI.equals(IRI.create(ont_meta))) { 
    			RemoveImport ri = new RemoveImport(ont, df.getOWLImportsDeclaration(importOntIRI));
    			manager.applyChange(ri);
    		}
     	}
    	
     	// merge the removed imported ontologies to the loaded one
     	for(OWLOntology importOnt: importOnts) {
     		IRI importOntIRI = importOnt.getOntologyID().getOntologyIRI();
    		if (importOntIRI.equals(IRI.create(iao_main)) || importOntIRI.equals(IRI.create(ont_meta))) {
    			OntologyManipulator.printPrefixNSs(manager, importOnt);
    			ont = OntologyManipulator.mergeToTargetOnt(manager, ont, importOnt);
    		}
     	} 
     	
     	OntologyManipulator.saveToFile(manager, ont, saveFilename);
		
	}
}
