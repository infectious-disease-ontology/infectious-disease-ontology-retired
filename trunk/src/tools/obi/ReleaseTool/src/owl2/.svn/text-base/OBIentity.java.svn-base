package owl2;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

public class OBIentity {
	/*
	public ArrayList<String> getLabels (OWLOntology ont, OWLClass cls){
		ArrayList<String> labels = new ArrayList<String>();
		
		OWLDataFactory df = manager.getOWLDataFactory();
		OWLAnnotationProperty labelProp = df.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
		
		for (OWLAnnotation annotation : cls.getAnnotations(ont, labelProp)) {
    		if (annotation.getValue() instanceof OWLLiteral) {
    			OWLLiteral val = (OWLLiteral) annotation.getValue();
    			labels.add(val.getLiteral());
    		}
    	}
    	
		return labels;
	}
	*/
	
	public static ArrayList<String> getOWLLiteralAnnotProps (OWLEntity cls, OWLDataFactory df, OWLOntology ont, OWLAnnotationProperty annotProp){
		ArrayList<String> propVals = new ArrayList<String>();
		
		for (OWLAnnotation annotation : cls.getAnnotations(ont, annotProp)) {
    		if (annotation.getValue() instanceof OWLLiteral) {
    			OWLLiteral val = (OWLLiteral) annotation.getValue();
    			propVals.add(val.getLiteral());
    		}
    	}
 	
		return propVals;
	}
	
	public static ArrayList<IRI> getIRIAnnotProps (OWLEntity cls, OWLDataFactory df, OWLOntology ont, OWLAnnotationProperty  annotProp){
		ArrayList<IRI> propVals = new ArrayList<IRI>();
		
		for (OWLAnnotation annotation : cls.getAnnotations(ont, annotProp)) {
    		if (annotation.getValue() instanceof IRI) {
    			IRI val = (IRI) annotation.getValue();
    			propVals.add(val);
    		}
    	}
    	
		return propVals;
	}
	
	public static String getLabel (OWLEntity cls, OWLOntology ont, OWLDataFactory df) {
		String label = cls.toString();

		OWLAnnotationProperty ap = df.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
		for (OWLAnnotation annotation : cls.getAnnotations(ont, ap)) {
			label = ((OWLLiteral) annotation.getValue()).getLiteral();
		}
		return label;
	}
	
	public static OWLLiteral getLabelOWLLiteral (OWLEntity cls, OWLOntology ont, OWLDataFactory df) {
		OWLLiteral labelOWLLiteral = null;

		OWLAnnotationProperty ap = df.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
		for (OWLAnnotation annotation : cls.getAnnotations(ont, ap)) {
    		if (annotation.getValue() instanceof OWLLiteral) {
    			labelOWLLiteral = (OWLLiteral) annotation.getValue();
    		}
		}
		return labelOWLLiteral;
	}
}
