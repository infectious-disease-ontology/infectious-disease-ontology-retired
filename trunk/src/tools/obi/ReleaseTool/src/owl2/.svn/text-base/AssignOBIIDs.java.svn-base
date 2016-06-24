package owl2;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.AutoIRIMapper;
import org.semanticweb.owlapi.util.OWLEntityRenamer;

public class AssignOBIIDs {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// locations of ontology files
 		String path = "C:/Documents and Settings/Jie/My Documents/Ontology/obi/trunk/src/ontology/";
 		String ontFile1 = path + "branches/obi.owl";
 		String ontFile2 = path + "branches/text-mining-experiment.owl";		
		String newOntFilename = path + "branches/obi_new.owl";		
		
 		// start number of ID
 		int offset = 1600;	
 		String base = "http://purl.obolibrary.org/obo/";
 		// ID space
 		String idSpace = "OBI";

 		// file contains newly assigned IDs, label, and type of terms
		String infoFileName = path + "branches/newIDs_20120328.tab";
		
		// id pattern
		String basePattern = "(http://purl.obolibrary.org/obo/)(.*)";
		Pattern oboBasePattern = Pattern.compile(basePattern);
		String idPattern = "^([A-Za-z]{2,10})(_)([0-9]{1,9})$";
		Pattern oboIdPattern = Pattern.compile(idPattern);
		
 		// Get hold of an ontology manager
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();	 
        
    	AutoIRIMapper mapper = new AutoIRIMapper(new File(path + "branches/"), false);
       	AutoIRIMapper mapper2 = new AutoIRIMapper(new File(path + "external/iao-bfo2/"), false);
       	manager.addIRIMapper(mapper);
    	manager.addIRIMapper(mapper2);
        OWLOntology ont = OntologyManipulator.load(ontFile1, manager); 
        
        AutoIRIMapper mapper3 = new AutoIRIMapper(new File(path + "external/"), false);
    	manager.addIRIMapper(mapper3);
        OWLOntology textOnt = OntologyManipulator.load(ontFile2, manager); 
        
        // get all terms ids
        HashSet<String> idStrs = OntologyVisitor.getTermsIRIStrings(manager, ont);
        HashSet<String> textIdStrs = OntologyVisitor.getTermsIRIStrings(manager, textOnt);
        idStrs.addAll(textIdStrs);
        
        // hash hold all the existing IDs of given idspace in an ontology
 		HashSet<Integer> existIDs = new HashSet<Integer>();		
 		// hash hold all the terms need a ID
 		HashMap<String, String> newTerms = new HashMap<String,String>();

        // find the terms with given idspace and the terms that need to assign new ids
    	Iterator<String> iterator = idStrs.iterator(); 
    	while (iterator.hasNext()){
    		String termIRIstr = iterator.next();
    		
    		// check whether it is OBO term IRI
    		Matcher m = oboBasePattern.matcher(termIRIstr);
    		if (m.find()) {
    			String termId = m.group(2);

    			// check whether it is a valid OBO term IRI
        		Matcher m2 = oboIdPattern.matcher(termId);
        		if (m2.find()) {
        			String space = m2.group(1);
        			
        			if (space.equals(idSpace)) {
        				Integer eID = Integer.valueOf(m2.group(3));
         				existIDs.add(eID);
        			}
        		} else {
        			newTerms.put(termIRIstr, "");
        		}
    		}
    	}
    	
    	Set<OWLOntology> onts = ont.getDirectImports();
    	onts.add(ont); 	
    	onts.add(textOnt);
    	OWLEntityRenamer renamer = new OWLEntityRenamer(manager, onts);
     	
    	// assign IDs to the newly added terms
    	iterator = newTerms.keySet().iterator();
    	while (iterator.hasNext()) {
    		String oldIRIstr = iterator.next();
    		String newId = getNewId(existIDs, offset);
    		String newIRIstr = base + idSpace + "_" + newId;
    		newTerms.put(oldIRIstr, newIRIstr);
    		offset = Integer.parseInt(newId) + 1;
    		List<OWLOntologyChange> changes = renamer.changeIRI(IRI.create(oldIRIstr), IRI.create(newIRIstr));
    		for (OWLOntologyChange change : changes) {
    			manager.applyChange(change);
    		}
    	}
    	
    	System.out.println("Total of " + newTerms.size() + " terms were assigned IDs" );
    	
    	// save ontology with newly assigned IDs
    	OntologyManipulator.saveToFile(manager, ont, newOntFilename);
    	
    	// save the information of newly added terms
    	OWLDataFactory df = manager.getOWLDataFactory();
    	
        try {
        	BufferedWriter out = new BufferedWriter(new FileWriter(infoFileName));
        	out.write("Old IRI\tNew ID\tLabel\tTerm Type\n");
        	
        	iterator = newTerms.keySet().iterator(); 
        	while (iterator.hasNext()){
        		String key = iterator.next();
        		String newIRIstr = newTerms.get(key);
        		out.write(key + "\t" + newIRIstr + "\t");
        		Set<OWLEntity> ents = ont.getEntitiesInSignature(IRI.create(newIRIstr));
        		if (ents.size() == 1) {
        			OWLEntity ent = ents.iterator().next();        			
        			out.write(OBIentity.getLabel(ent, ont, df) + "\t" + ent.getEntityType().getName() + "\n");
        		} else {
        			out.write("Something wrong, no entity or more than one entity with given IRI\t" + ents.size() + "\n");
        		}
        	}
        	
        	out.close();
        }
        catch (IOException e) {
        	System.out.println("Exception ");
        }   	

        System.out.println("Details of newly added terms saved in the file: " + infoFileName);
	}
	
	public static String getNewId(HashSet<Integer> existIDs, int offset) {
		int newID = offset;
		
		while (existIDs.contains(new Integer(newID))) {
			newID ++;
		}

		// 7 digits ID
		String s = "0000000" + newID; 
					
		return s.substring(s.length()-7);
	}
}
