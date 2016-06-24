package owl2;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

public class OntoView {
	public final static int INFER = 1;
	public final static int ANNOT = 2;
	
	/**
	 * Author: Jie Zheng <br>
	 * University of Pennsylvania <br>
	 * Date: Nov-1-2011 <br>
	 * 
 	 * @param -type: INFER (generate inferred hierarchy), ANNOT (create annotation layer)
	 * @param -sourceOnt
	 */
	public static void main(String[] args) {
		// parameters for generate inferred hierarchy target ontology
		String targetOntStr = null;														// required
		String outFilename  = null;														// required
		String reasonerName = "hermit";													// optional, can use hermit, pellet and FACT ++ reasoners
		// parameters for generating annotation layer ontology
		String annotOntURIStr 	= null;													// required
		String annotOntFilename = null;													// required
		String annotPropStr = "http://www.geneontology.org/formats/oboInOwl#inSubset";	// required
		String annotPropVal = null;														// required
		String termFilename = null;														// required
		String sourceOntStr = null;														// optional 
		
		// parse passed arguments
		int progType = 0;			
		int i = 0;
		while (i < args.length) {
			String aName = args[i].trim();
			// System.out.println(aName);			
			i++;
			
			if (aName.equalsIgnoreCase("INFER")) {
				progType = INFER;
			} else if (aName.equalsIgnoreCase("ANNOT")) {
				progType = ANNOT;
			} else if (aName.equalsIgnoreCase("--reasoner")) {
				if ( args[i].equalsIgnoreCase("pellet")) {
					reasonerName = args[i];
				}
				i ++;
			} else if (aName.equalsIgnoreCase("--targetURI ")) {
				targetOntStr = args[i];
				i ++;
			} else if (aName.equalsIgnoreCase("--outFilename")) {
				outFilename = args[i];
				i ++;
			} else if (aName.equalsIgnoreCase("--annotOntURI")) {
				annotOntURIStr = args[i];
				i ++;
			} else if (aName.equalsIgnoreCase("--annotOntFile")) {
				annotOntFilename = args[i];
				i ++;
			} else if (aName.equalsIgnoreCase("--annotProp")) {
				if (args[i] != null)	annotPropStr = args[i];
				i ++;
			}else if (aName.equalsIgnoreCase("--annotPropVal")) {
				annotPropVal = args[i];
				i ++;
			}else if (aName.equalsIgnoreCase("--termFile")) {
				termFilename = args[i];
				i ++;
			}else if (aName.equalsIgnoreCase("--sourceOnt")) {
				sourceOntStr = args[i];
				i ++;
			}else if (aName.equalsIgnoreCase("-help")) {
				System.out.print("The program can perform different actions: \n" +
						"\tINFER: generate inferred hierarchy\n" +
						"\tANNOT: generate annotation layer\n" +
						"Some parameters are needed for each action.\n" +
						"Parameters needed for generating inferred hierarchy:\n" +
						"\t--targetOnt (required): URI or physical location of ontology for classification\n" +
						"\t--outFilename (required): filename (including full path) of the inferred ontology\n" +
						"\t--reasoner (optional): reasoner used for classification, default set as hermit, current only support hermit and pellet\n" +
						"Parameters needed for generating annotation layer ontology:\n" +
						"\t--annotProp (optional): URI of an annotation property used to tag specified view term, default is \"obo:inSubset\"\n" +
						"\t--annotPropVal (optional): value will be added the given annotation property to specified view terms\n" +
						"\t--termFile (required): a tab delimited file that marked the terms need user preferred label signatures and the label user preferred for a given term\n" +
						"\t--annotOntURI (required): ontology URI of the user preferred label annotation layer\n" +
						"\t--anootOntFile (required): physical location of the user preferred label annotation layer ontology where it will be saved to\n" +
						"\t--sourceOnt (optional): URI or physical of an ontology, used to check whether given annotation property and terms exists in it\n");
			}
		}
		
		// progType = ANNOT;
		// annotOntFilename = "C:/Documents and Settings/Jie/My Documents/SLIM_subset.owl";													
		// annotOntURIStr = "http://purl.obolibrary.org/obo/obi/SLIM_subset.owl";	
		// annotPropStr = "http://purl.obolibrary.org/obo/OBI_9991119";
		// annotPropStr = "http://www.geneontology.org/formats/oboInOwl#inSubset";	
		// annotPropVal = "SLIM";														
		// termFilename = "C:/Documents and Settings/Jie/My Documents/Manuscript/2011_ontoDog/ontoDog/OBI_SLIM.txt";														
		// sourceOntStr = "C:/Documents and Settings/Jie/My Documents/Manuscript/2011_ontoDog/ontoDog/FGEDview/obi.owl";														 
		progType = INFER;
		//targetOntStr = "C:/Documents and Settings/Jie/My Documents/pizza.owl";
		targetOntStr = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/releases/2011-07-20/branches/obi.owl";
		outFilename = "C:/Documents and Settings/Jie/My Documents/obi_merged.owl";
		String outFilename2 = "C:/Documents and Settings/Jie/My Documents/obi_withInferred.owl";
		
	    switch (progType) {
	        case INFER:
	        	System.out.println("Generate inferred hierarchy of the given ontology");
	        	
	        	if ((targetOntStr != null)&& (outFilename != null)) {
                	OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	
                	 
                	// load source ontology	                	
                	OWLOntology targetOnt = OntologyManipulator.loadFromFile(targetOntStr, manager);
              	
                	String targetURIstr = targetOnt.getOntologyID().getOntologyIRI().toString();
                	String inferURIstr = null;
                	String mergeURIstr = null;
                	
                	if (targetURIstr.endsWith(".owl")) {
                		String subStr = targetURIstr.substring(0, (targetURIstr.length()-4));
                		inferURIstr = subStr + "_inferred.owl";
                		mergeURIstr = subStr + "_merged.owl";
                	} else {
                		inferURIstr = targetURIstr + "_inferred.owl";
                		mergeURIstr = targetURIstr + "_merged.owl";
                	}
                	// System.out.print("\nInferred ontoloyg URI " + inferURIstr + "\nmerged ontology URI "+ mergeURIstr + "\n\n");
                	
                	//OWLOntology inferredOnt = OWLReasonerRunner.getOntologyWithInferredSuperClasses (manager, targetOnt, inferURIstr, reasonerName);
                	//OntologyManipulator.saveToFile(manager, inferredOnt, outFilename2);
                	OWLReasoner reasoner = OWLReasonerRunner.runReasoner(manager, targetOnt, reasonerName);
                	OWLOntology mergedOnt = OWLReasonerRunner.getCleanedOntologyWithInferredSuperClasses(manager, targetOnt, mergeURIstr, reasoner);
                	OntologyManipulator.saveToFile(manager, mergedOnt, outFilename);
                	/*
                	for(OWLOntology ontInMan: manager.getOntologies()) {
                		System.out.println(ontInMan);
                	}
                	*/
                   	//OWLOntology mergedOnt = OntologyManipulator.merge(manager, mergeURIstr);
               	
                	// Save the inferred ontology. (Replace the URI with one that is appropriate for your setup)
            	    //OntologyManipulator.saveToFile(manager, mergedOnt, outFilename);	               	

	        	} else {
					System.out.println("You must provide both ontology for classification and location of where the output file will be saved to.");	        		
	        	}
	            break;
	        case ANNOT:
	        	System.out.println("Create annotation layer");
	        	
	        	if ((annotOntURIStr != null) && (annotOntFilename != null) && (annotPropStr != null) && (termFilename != null)) {
	                if (IRI.create(annotOntURIStr).isReservedVocabulary()) {
	                	System.out.println("Error! You are using a reserved IRI; " + annotOntURIStr + " for the annotation property ontology.");
	                } else {	        	
	                	OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	
 
             	        // annotation layer ontology
             	        OWLOntology annotOnt = OntologyManipulator.create(manager, annotOntURIStr);
             	        
             	        String annotPropURIstr = annotPropStr; 
	                
	                	OWLOntology sourceOnt = null;
	                	if (sourceOntStr != null) {
	                		// load source ontology	                	
	                		sourceOnt = OntologyManipulator.loadFromFile(sourceOntStr, manager);
	                	}
	                		
	                	// check whether annotation property exists in source ontology
	                	if (sourceOnt != null && !sourceOnt.containsAnnotationPropertyInSignature(IRI.create(annotPropURIstr), true)) {                		
	                		System.out.println("\nThe annotation property -" + annotPropStr + ", does not exist in the source ontology.");
	                	} 

	                	System.out.print("\nAdd annotations with Annotation Property which is: " + annotPropURIstr + "\n");
	                	
	                	if (sourceOnt == null)		System.out.println("Warning! axioms will be added without checking whether annotation property or terms exist in the source ontology.");
	             
	                	// add annotations to specified terms
	                	annotOnt = AnnotPropLayerAdder.add(manager, annotOnt, sourceOnt, annotPropURIstr, annotPropVal, termFilename);
	                	
	                	// save ontology
	                	OntologyManipulator.saveToFile(manager, annotOnt, annotOntFilename);
	                }        		
	        	} else {
					System.out.println("You must provide all the following parameters: annotOntURI, annotOntFile, annotProp, and termFile parameters. For details, please type -help.");	    			
	    		}
	            break;
	        default: 
				System.out.println("Please specify what you want to do: \n\tINFER - create inferred hierarchy\n\tANNOT - generate annotation layer ontology \n");
				break;	        	
	    }   
	}
}
