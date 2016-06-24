package owl2;

import java.util.ArrayList;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

public class QCChecker {
	private OWLOntology ontology;
	private OWLOntologyManager man;
	
	QCChecker () {
		this.ontology = null;
		this.man = null;
	}
	
	QCChecker (OWLOntology ontology, OWLOntologyManager man){
		this.ontology = ontology;
		this.man = man;
	}
		
	void setOntology (OWLOntology ontology){
		this.ontology = ontology;
	}
	
	void setManager (OWLOntologyManager man){
		this.man = man;
	}

	
	public void checkMultiplePropVals(IRI annotPropIRI) {
		OWLDataFactory df = man.getOWLDataFactory();
		for (OWLClass cls : ontology.getClassesInSignature()) {
			ArrayList<String> labels = OBIentity.getOWLLiteralAnnotProps(df, ontology, cls, annotPropIRI);
			if (labels.size() > 1) {
				System.out.println(cls + " has multiple labels: " + labels.get(0));
			}
		}
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	
    	
    	// load ontology
    	// loader.loadFromFile("C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2011-04-20/merged/obi-fgedView-preferred.owl", manager);
        OWLOntology ont = OntologyManipulator.loadFromFile(Config.OBI_FILE_NAME, manager);
    	
    	QCChecker checker = new QCChecker(ont, manager);
    	
    	// check whether there is multiple rdfs:label
    	checker.checkMultiplePropVals(OWLRDFVocabulary.RDFS_LABEL.getIRI());
	}
}
