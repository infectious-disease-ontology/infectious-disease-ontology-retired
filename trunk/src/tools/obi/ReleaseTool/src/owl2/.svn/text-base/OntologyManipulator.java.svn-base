package owl2;

import org.coode.xml.OWLOntologyXMLNamespaceManager;
import org.semanticweb.owlapi.io.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.util.SimpleIRIMapper;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Adapted from Matthew Horridge's OWL API examples by Jie Zheng <br>
 * University of Pennsylvania <br>
 * Date: Nov-1-2011 <br>
 */
public class OntologyManipulator {
	private static OWLOntology ont = null;
	
	// load an ontology
	public static OWLOntology load (String str, OWLOntologyManager manager) {
		if (new File(str).exists()) {
		    // load ontology from a file
			ont = loadFromFile(str, manager);
		} else {
		    // File or directory does not exist
			ont = loadFromWeb(str, manager);
		}

		return ont;
	}
	
	// load an ontology from the web
	public static OWLOntology loadFromWeb (String url, OWLOntologyManager manager) {
		try {
            // Get hold of an ontology manager
            // OWLOntologyManager manager = OWLManager.createOWLOntologyManager();		

            // Let's load an ontology from the web
            IRI iri = IRI.create(url);
            ont = manager.loadOntologyFromOntologyDocument(iri);
            System.out.println("Loaded ontology: " + ont + " from web.");  
        }
        catch (OWLOntologyCreationIOException e) {
            // IOExceptions during loading get wrapped in an OWLOntologyCreationIOException
            IOException ioException = e.getCause();
            if (ioException instanceof FileNotFoundException) {
                System.out.println("Could not load ontology. File not found: " + ioException.getMessage());
            }
            else if (ioException instanceof UnknownHostException) {
                System.out.println("Could not load ontology. Unknown host: " + ioException.getMessage());
            }
            else {
                System.out.println("Could not load ontology: " + ioException.getClass().getSimpleName() + " " + ioException.getMessage());
            }
        }
        catch (UnparsableOntologyException e) {
            // If there was a problem loading an ontology because there are syntax errors in the document (file) that
            // represents the ontology then an UnparsableOntologyException is thrown
            System.out.println("Could not parse the ontology: " + e.getMessage());
            // A map of errors can be obtained from the exception
            Map<OWLParser, OWLParserException> exceptions = e.getExceptions();
            // The map describes which parsers were tried and what the errors were
            for (OWLParser parser : exceptions.keySet()) {
                System.out.println("Tried to parse the ontology with the " + parser.getClass().getSimpleName() + " parser");
                System.out.println("Failed because: " + exceptions.get(parser).getMessage());
            }
        }
        catch (UnloadableImportException e) {
            // If our ontology contains imports and one or more of the imports could not be loaded then an
            // UnloadableImportException will be thrown (depending on the missing imports handling policy)
            System.out.println("Could not load import: " + e.getImportsDeclaration());
            // The reason for this is specified and an OWLOntologyCreationException
            OWLOntologyCreationException cause = e.getOntologyCreationException();
            System.out.println("Reason: " + cause.getMessage());
        }
        catch (OWLOntologyCreationException e) {
            System.out.println("Could not load ontology: " + e.getMessage());
        }
        return ont;
	}

	// load ontology from a local file
	public static OWLOntology loadFromFile (String filename, OWLOntologyManager manager)
    {
	    try {
            // Get hold of an ontology manager
            // OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

            File file = new File(filename);

            // Now load the local copy
            ont = manager.loadOntologyFromOntologyDocument(file);
            System.out.println("Loaded ontology: " + ont + " from a file: " + filename);
        }
        catch (OWLOntologyCreationIOException e) {
            // IOExceptions during loading get wrapped in an OWLOntologyCreationIOException
            IOException ioException = e.getCause();
            if (ioException instanceof FileNotFoundException) {
                System.out.println("Could not load ontology. File not found: " + ioException.getMessage());
            }
            else if (ioException instanceof UnknownHostException) {
                System.out.println("Could not load ontology. Unknown host: " + ioException.getMessage());
            }
            else {
                System.out.println("Could not load ontology: " + ioException.getClass().getSimpleName() + " " + ioException.getMessage());
            }
        }
        catch (UnparsableOntologyException e) {
            // If there was a problem loading an ontology because there are syntax errors in the document (file) that
            // represents the ontology then an UnparsableOntologyException is thrown
            System.out.println("Could not parse the ontology: " + e.getMessage());
            // A map of errors can be obtained from the exception
            Map<OWLParser, OWLParserException> exceptions = e.getExceptions();
            // The map describes which parsers were tried and what the errors were
            for (OWLParser parser : exceptions.keySet()) {
                System.out.println("Tried to parse the ontology with the " + parser.getClass().getSimpleName() + " parser");
                System.out.println("Failed because: " + exceptions.get(parser).getMessage());
            }
        }
        catch (UnloadableImportException e) {
            // If our ontology contains imports and one or more of the imports could not be loaded then an
            // UnloadableImportException will be thrown (depending on the missing imports handling policy)
            System.out.println("Could not load import: " + e.getImportsDeclaration());
            // The reason for this is specified and an OWLOntologyCreationException
            OWLOntologyCreationException cause = e.getOntologyCreationException();
            System.out.println("Reason: " + cause.getMessage());
        }
        catch (OWLOntologyCreationException e) {
            System.out.println("Could not load ontology: " + e.getMessage());
        }
        return ont;
    }

	public static OWLOntology create (OWLOntologyManager manager, String iri) {
		try {
	    	IRI ontIRI = IRI.create(iri);
	    	ont = manager.createOntology(ontIRI);
	    	System.out.println("A new ontology is created, IRI is: " + iri);
		} catch (OWLException e) {
			e.printStackTrace();
		}
		
		return ont;
	}

	public static OWLOntology createAnonymous (OWLOntologyManager manager) {
		try {
	    	ont = manager.createOntology();
	    	System.out.println("A new anonymous ontology is created");
		} catch (OWLException e) {
			e.printStackTrace();
		}
		
		return ont;
	}
	public static OWLOntology createWithDocMap (OWLOntologyManager manager, String iri, String filePath) {
		try {
	    	IRI ontIRI = IRI.create(iri);
	    	IRI docIRI = IRI.create(filePath);
	    	SimpleIRIMapper mapper = new SimpleIRIMapper(ontIRI, docIRI);
	    	manager.addIRIMapper(mapper);
	    	ont = manager.createOntology(ontIRI);
	    	System.out.println("A new ontology is created, IRI is: " + iri);
		} catch (OWLException e) {
			e.printStackTrace();
		}
		
		return ont;
	}
	
	public static OWLOntologyManager addDocIRIMapper(OWLOntologyManager manager, String iri, String filePath) {
    	IRI ontIRI = IRI.create(iri);
    	IRI docIRI = IRI.create(filePath);
    	SimpleIRIMapper mapper = new SimpleIRIMapper(ontIRI, docIRI);
    	manager.addIRIMapper(mapper);	
		
    	return manager;
	}
	
	public static OWLOntology createFromOWLontologies (OWLOntologyManager manager, String iri, Set<OWLOntology> onts) {
		try {
	    	IRI ontIRI = IRI.create(iri);
	    	ont = manager.createOntology(ontIRI, onts);
	    	System.out.println("A new ontology is created and copied axioms from a set of given ontologies, IRI is: " + iri);
		} catch (OWLException e) {
			e.printStackTrace();
		}
		
		return ont;
	}
	
	public static void saveToFile(OWLOntologyManager manager, OWLOntology ont, String filename) {
	   try {
		   // By default ontologies are saved in the format from which they were loaded.  In this case the
		   // ontology was loaded from an rdf/xml file
	       // We can get information about the format of an ontology from its manager
	       OWLOntologyFormat format = manager.getOntologyFormat(ont);
	       System.out.println("Save ontology in the format: " + format);
	
		   // Now save a local copy of the ontology.  (Specify a path appropriate to your setup)
		   File file = new File(filename);
		   manager.saveOntology(ont, IRI.create(file.toURI()));
				   
		   System.out.println("The ontology has been save on: " + filename);

		   /*

		   // We can save the ontology in a different format
		            // Lets save the ontology in owl/xml format
		            OWLXMLOntologyFormat owlxmlFormat = new OWLXMLOntologyFormat();
		            // Some ontology formats support prefix names and prefix IRIs.  In our case we loaded the pizza ontology
		            // from an rdf/xml format, which supports prefixes.  When we save the ontology in the new format we will
		            // copy the prefixes over so that we have nicely abbreviated IRIs in the new ontology document
		            if(format.isPrefixOWLOntologyFormat()) {
		                owlxmlFormat.copyPrefixesFrom(format.asPrefixOWLOntologyFormat());
		            }
		            manager.saveOntology(pizzaOntology, owlxmlFormat, IRI.create(file.toURI()));

		            // We can also dump an ontology to System.out by specifying a different OWLOntologyOutputTarget
		            // Note that we can write an ontology to a stream in a similar way using the StreamOutputTarget class
		            OWLOntologyDocumentTarget documentTarget = new SystemOutDocumentTarget();
		            
		            // Try another format - The Manchester OWL Syntax
		            ManchesterOWLSyntaxOntologyFormat manSyntaxFormat = new ManchesterOWLSyntaxOntologyFormat();
		            if(format.isPrefixOWLOntologyFormat()) {
		                manSyntaxFormat.copyPrefixesFrom(format.asPrefixOWLOntologyFormat());
		            }
		           manager.saveOntology(pizzaOntology, manSyntaxFormat, new SystemOutDocumentTarget());

			    */
	   }
	   catch (OWLOntologyStorageException e) {
		   System.out.println("Could not save ontology: " + e.getMessage());
	   }
	}
	
	public static OWLOntology merge (OWLOntologyManager manager, String mergeOntURIstr) {
		OWLOntology mergedOnt = null;
		
		OWLOntologyMerger merger = new OWLOntologyMerger(manager);
		
		try {
			mergedOnt = merger.createMergedOntology(manager, IRI.create(mergeOntURIstr));
		} catch (OWLOntologyCreationException e) {
            System.out.println("Could not create merged ontology: " + e.getMessage());
        }
		
		return mergedOnt; 
	}
	
	public static OWLOntology mergeToTargetOnt (OWLOntologyManager manager, OWLOntology targetOnt, OWLOntology ont) {
		Set<OWLAxiom> axs = ont.getAxioms();
		for(OWLAxiom ax : axs) {
	    	if (!targetOnt.containsAxiom(ax, true)) {
	        	manager.applyChange(new AddAxiom(targetOnt, ax));
	        }
	    }
		
		IRI targetOntIRI = targetOnt.getOntologyID().getOntologyIRI();
		IRI ontIRI = ont.getOntologyID().getOntologyIRI();
		
		System.out.println("All axioms in ontology " + ontIRI.toString() + " have been merged to the ontology " + targetOntIRI.toString());
		
		return targetOnt;
	}
	
	public static OWLOntology setOntologyID (OWLOntologyManager manager, OWLOntology ont, String newIRIstr) {
		OWLOntologyID oldID = ont.getOntologyID();
		String oldIRIstr = oldID.getOntologyIRI().toString();
		
		if (newIRIstr.equals(oldIRIstr)) {
			System.out.println("The new IRI: " + newIRIstr + " is same to the original one.");
		} else {
			OWLOntologyID newID = new OWLOntologyID(IRI.create(newIRIstr));		
			SetOntologyID setID = new SetOntologyID(ont, newID);
			manager.applyChange(setID);
			
			// still need to work on this part
			// change the namespace if it is same as the ontologyIRI
			OWLOntologyXMLNamespaceManager nsManager = new OWLOntologyXMLNamespaceManager(manager, ont);
			Map<String, String> prefixNSs = nsManager.getPrefixNamespaceMap();
			Set s = prefixNSs.entrySet();
			Iterator it = s.iterator();

	        while(it.hasNext())
	        {
	            // key=value separator this by Map.Entry to get key and value
	            Map.Entry m =(Map.Entry)it.next();

	            // getKey is used to get key of Map
	            String prefix = (String) m.getKey();

	            // getValue is used to get value of key in Map
	            String namespace = (String) m.getValue();

	            System.out.println("Prefix:"+ prefix +"  Namespace:" + namespace);
	        }
		}
		
		return ont;
	}
	
	public static void printPrefixNSs (OWLOntologyManager manager, OWLOntology ont) {
		OWLOntologyID id = ont.getOntologyID();
		System.out.println("Ontology: " + id.getOntologyIRI().toString());

 		IRI documentIRI = manager.getOntologyDocumentIRI(ont);
 		System.out.println(" from: " + documentIRI + "\n");
		
		OWLOntologyXMLNamespaceManager nsManager = new OWLOntologyXMLNamespaceManager(manager, ont);
		Map<String, String> prefixNSs = nsManager.getPrefixNamespaceMap();
		Set s = prefixNSs.entrySet();
		Iterator it = s.iterator();

	    while(it.hasNext()) {
	    	// key=value separator this by Map.Entry to get key and value
	        Map.Entry m =(Map.Entry)it.next();

	        // getKey is used to get key of Map
	        String prefix = (String) m.getKey();

	        // getValue is used to get value of key in Map
	        String namespace = (String) m.getValue();

	        System.out.println("Prefix:"+ prefix +"  Namespace:" + namespace);
	    }
	    
	    System.out.println("\n");
	}
}
