/**
 * @author Melanie
 * Feb 21, 2008
 * 
 * Filename    : OBIMerger.java
 * Copyright (C)  Melanie Courtot 2008
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more
 * details. http://www.gnu.org/licenses/gpl.txt
 * Melanie Courtot
 * Terry Fox Laboratory, BC Cancer Research Centre. Vancouver, BC V5Z 1L3  Canada. mcourtot@bccrc.ca
 */

package ca.bccrc.obi;



import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


import org.mindswap.pellet.jena.OWLReasoner;
import org.mindswap.pellet.jena.PelletReasonerFactory;


//import com.hp.hpl.jena.ontology.OntDocumentManager;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.rdf.model.impl.StatementImpl;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;




public class OBIMerger {

	//declaration of the variables
	//TODO externalize those (properties file)
	//the OBI namespace
	//private static String OBINs = "http://obi.sourceforge.net/ontology/OBI.owl#";
	private static String OBINs = "http://purl.obolibrary.org/obo/";
	//the OBI xmlbase element
	//private static String xmlbase = "http://obi.sourceforge.net/ontology/OBI.owl";
	private static String xmlbase = "http://purl.obolibrary.org/obo/";
	//the name of the file containing the declaration of all the imports
	private static String obi = "obi.owl";
	//the obiPath - this is the path for the branches files
	//private static String obiPath = "http://obi.sourceforge.net/ontology/OBI/";
	private static String obiPath = "http://purl.obolibrary.org/obo/obi/";

	//the Ontology URI
	private static String obiURI = "http://purl.obolibrary.org/obo/obi.owl";



	/**
	 * Returns list of branches names as referenced in the OBI file
	 * @param obi - the file containing the imports of all the branches, e.g. obi.owl
	 * @param physicalURI - the physical path to the directory containing the files
	 * @return - a list of the branches names, e.g. "Biomaterial"
	 */
	public static List<String> getBranchesNames(String obi, String physicalURI){
		List<String> branchesNames = new ArrayList<String>();
		try{
			branchesNames.add("Biomaterial");
			branchesNames.add("externalDerived");
			branchesNames.add("external");
			branchesNames.add("Role");
			branchesNames.add("InstrumentAndPart");
			//branchesNames.add("TheRest");
			branchesNames.add("Relations");
			branchesNames.add("PlanAndPlannedProcess");
			branchesNames.add("AnnotationProperty");

			branchesNames.add("OBI-Function");
			branchesNames.add("DataTransformation");

			branchesNames.add("Quality");
			branchesNames.add("Obsolete");

			branchesNames.add("DigitalEntityPlus");
			branchesNames.add("DataFormatSpecification");
			branchesNames.add("instances/dataTransformationInstances");
			/*
			 * remove those for testing - need to be included though for release
			 */
		branchesNames.add("disjoints");
		branchesNames.add("inferred-superclasses");
			////////	branchesNames.add("assumed-individuals");
			branchesNames.add("external-byhand");
			//instances
			branchesNames.add("instances/softwareInstances");
			branchesNames.add("instances/organizationInstances");
			/* we don't include the quick-id file in the release process. 
			 * It does get OBI IDs assigned via list-purls lisp script, but is discarded straight afterwards (not even considered for disjoints)
			 */
			//branchesNames.add("obi-quick-id");

		}catch (Exception e){
			System.err.println("Error: " + e.getMessage());
		}


		return branchesNames;
	}

	/*
	 * 
	 * This method has now been replaced by a call via the owlapi, in EquivalentClassesChecker.java
	 * 
	 * 
	 */
	public static boolean checkConsistency(String ontologyPath)	{


		boolean toCommit = true;
		FileInputStream fstream = null;
		try {
			fstream = new FileInputStream(ontologyPath);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		//we create the model with Pellet spec
		OntModel model = ModelFactory.createOntologyModel(PelletReasonerFactory.THE_SPEC);

		// read in the ontology:
		model.read(fstream, obiURI);



		//get the report
		ValidityReport report = model.validate();
		Iterator<?> it = report.getReports();
		while (it.hasNext()){
			System.out.println("report: "+it.next().toString());
			toCommit = false;
		}

		//we check the OWL level
		//if different from DL we don't commit
		OWLReasoner reasoner = new OWLReasoner();
		reasoner.load(model);
		String level = reasoner.getLevel();
		if (!level.equalsIgnoreCase("DL"))
		{
			toCommit = false;
			System.out.println("level"+level);
		}
		/* test writing inference with pellet
		 * 
		 * System.out.println("level"+level);//TO REMOVE!!!
		try {
			reasoner.extractModel( true ).write(new FileOutputStream("/Users/mcourtot/Desktop/20081223/merged/OBI_inferred.owl",true),"UTF8");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}*/
		//return false if there has been any problem, or true if everything is fine
		return toCommit;
	}


	/**
	 * Writes the OntModel in the newFile using an RDFWriter
	 * @param newFile the File to will be written
	 * @param model the OntModel to be written
	 */
	public static void writeFile(File newFile, OntModel model){
		//writes the file
		OutputStreamWriter out = null;
		try {
			out = new OutputStreamWriter(new FileOutputStream(newFile,true),"UTF8");
		} catch (UnsupportedEncodingException e) {
			System.out.println(e);
			throw new RuntimeException(e);
		} catch (FileNotFoundException e) {
			System.out.println(e);
			throw new RuntimeException(e);
		}


		//creates the RDF writer
		RDFWriter writer = model.getWriter("RDF/XML-ABBREV");

		//sets the writer properties
		writer.setProperty("xmlbase", xmlbase);
		writer.setProperty("showXmlDeclaration","true");
		//absolute uris: we want rdf:about http://purl.obofoundry.org and not ../
		writer.setProperty("relativeURIs","");
		//write the model into the out file
		writer.write(model, out, OBINs);

	}

	public static void addImports(Ontology ont, OntModel owlModel)	{
		//we add the external imports
		//we add those as Ontology Resource, to get the proper syntax for the imports, otherwise Pellet complains OWL full
		//NOTE: this causes a display problem in Protege 3, see https://mailman.stanford.edu/pipermail/protege-owl/2007-December/004728.html
		ont.addImport(owlModel.createOntology("http://purl.obolibrary.org/obo/iao/dev/iao.owl"));
		ont.addImport(owlModel.createOntology("http://www.ifomis.org/bfo/1.1"));
		//ont.addImport(owlModel.createOntology("http://purl.org/obo/owl/OBO_REL")); 
		//ont.addImport(owlModel.createResource("http://purl.org/obo/owl/relationship"));
		ont.addImport(owlModel.createOntology("http://www.obofoundry.org/ro/ro.owl"));
		ont.addImport(owlModel.createOntology("http://purl.org/obo/owl/ro_bfo_bridge/1.1"));
		ont.addImport(owlModel.createOntology("http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl"));



	}


/*	public static void addProtegeFriendlyImports(Ontology ont, OntModel owlModel)	{
		//we add the external imports
		//we add those as Resource, to get the proper syntax for the imports, and proper display in protege
		//NOTE: this will cause Pellet to classify as OWL Full (Untyped Ontology)
		ont.addImport(owlModel.createResource("http://purl.obolibrary.org/obo/iao/dev/iao.owl"));
		ont.addImport(owlModel.createResource("http://www.ifomis.org/bfo/1.1"));
		//ont.addImport(owlModel.createResource("http://purl.org/obo/owl/OBO_REL"));
		//ont.addImport(owlModel.createResource("http://purl.org/obo/owl/relationship"));
		ont.addImport(owlModel.createResource("http://www.obofoundry.org/ro/ro.owl"));
		ont.addImport(owlModel.createResource("http://purl.org/obo/owl/ro_bfo_bridge/1.1"));
		ont.addImport(owlModel.createResource("http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl"));

	}
*/


	public static OntModel buildOWLModel (String physicalURI){
		//create the Jena model
		OntModel owlModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);	

		//we read all the branches into the owlModel to merge them
		List<String> branchesnames = getBranchesNames(obi,physicalURI);

		for(String s : branchesnames) {

			try {
				String branchPath = physicalURI +s+".owl";
				//System.out.println("branch names: "+ branchPath);
				String ontoURI = obiPath+s+".owl";
				Ontology ont = owlModel.createOntology(ontoURI);

				//we define alternative entry for the ontologies
				//this allows us to map for example purl.obofoundry.org/obo/obi/Biomaterial.owl to its physical location
				//needed in the case where for example Relations.owl imports Biomaterial.owl.
				//Interestingly, if I do that, the prefix are not removed anymore on the final version...
				//AND if I get the java exception I guess Jena skips the import and I'm not getting the namespace problem with Protege
				//OntDocumentManager OntDocumentManager = owlModel.getDocumentManager();
				//OntDocumentManager.addAltEntry(ontoURI, branchPath);


				//owlModel.read(new FileInputStream(branchPath), obiPath+s+".owl");

				//we read each branch into the model to merge them
				//problem if I do that is that then for the namespaces defined in branch files the prefixes are not kept.
				//for example xmlns:cell="http://purl.org/obo/owl/CL#" disappears from the final merge, which gives a p1 namespace in Protege.
				//other option is to read and write the branch file, and then read it in the total merged file
				OntModel branchModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);	
				branchModel.read(new FileInputStream(branchPath), obiPath+s+".owl");
				//System.out.println("reading : "+s);
				owlModel.add(branchModel);


				//we remove the individual properties per branch (e.g. version)
				ont.removeProperties();



			}
			catch (Exception ex) {
				ex.printStackTrace();
				System.exit(1);
			}
		}
		return owlModel;
	}





	public static OntModel buildMergedFiles(String physicalURI, boolean ProtegeFriendly){


		//creates the ontology model
		OntModel owlModel = buildOWLModel(physicalURI);
		//creates the ontology
		Ontology ont = owlModel.createOntology(obiURI);
		//Protege-Friendly version?
		/*if(ProtegeFriendly) addProtegeFriendlyImports(ont, owlModel);
		else */addImports(ont, owlModel);


		//we also add the AnnotationProperty for the defaultLanguage of Protege, otherwise Pellet complains OWL full
		owlModel.createAnnotationProperty("http://protege.stanford.edu/plugins/owl/protege#defaultLanguage");

		//we add the information regarding creators etc that is in the file TheRest.owl
		//have been moved in obi.owl
		String theRestPath = physicalURI+"obi.owl";
		OntModel owlModel2 = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		Ontology ont2 = owlModel2.createOntology(obiURI);

		try {
			owlModel2.read(new FileInputStream(theRestPath), obiURI);
		} catch (FileNotFoundException e) {
			System.out.println("Can't read obi.owl");
			e.printStackTrace();
		}

		StmtIterator properties = ont2.listProperties();
		StatementImpl stmt;
		System.out.println("properties");
		while (properties.hasNext())
		{
			stmt = (StatementImpl) properties.next();

			//we create a resource in the OBI namespace for dc properties only
			//we don't want to be getting the imports declaration (especially not the import declaration of all the branch files...)
			//    <protege:defaultLanguage rdf:datatype="http://www.w3.org/2001/XMLSchema#string">en</protege:defaultLanguage>
			System.out.println("properties: "+ stmt + stmt.getPredicate());
			if (stmt.getPredicate().toString().matches("http://purl.org/dc/elements/1.1/(.*)") || stmt.getPredicate().toString().matches("http://protege.stanford.edu/(.*)")){
				
				Resource obins = owlModel2.createResource(obiURI);
				StatementImpl stmt2 = new StatementImpl(obins,stmt.getPredicate(),stmt.getObject());
				owlModel.add(stmt2);
			}
		}



		//set the namespaces prefix mapping
		//we want to have "clean" namespaces declarations, and avoid for example the j:0 from Protege
		//TODO there is probably a way to set those via a Map
		owlModel.getGraph().getPrefixMapping().setNsPrefix("xsd", "http://www.w3.org/2001/XMLSchema#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");

		owlModel.getGraph().getPrefixMapping().setNsPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("owl", "http://www.w3.org/2002/07/owl#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("daml", "http://www.daml.org/2001/03/daml+oil#");

		owlModel.getGraph().getPrefixMapping().setNsPrefix("dcterms", "http://purl.org/dc/terms/");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("dc", "http://purl.org/dc/elements/1.1/");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("protege", "http://protege.stanford.edu/plugins/owl/protege#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("protege-dc", "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("oboInOwl", "http://www.geneontology.org/formats/oboInOwl#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("oborel","http://purl.org/obo/owl/OBO_REL#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("relationship","http://purl.org/obo/owl/relationship#");


		owlModel.getGraph().getPrefixMapping().setNsPrefix("bfo", "http://www.ifomis.org/bfo/1.1#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("robfo", "http://purl.org/obo/owl/ro_bfo_bridge/1.1#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("snap", "http://www.ifomis.org/bfo/1.1/snap#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("span", "http://www.ifomis.org/bfo/1.1/span#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("ro", "http://www.obofoundry.org/ro/ro.owl#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("rotoo", "http://purl.org/obo/owl/ro#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("pato", "http://purl.org/obo/owl/PATO#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("cell", "http://purl.org/obo/owl/CL#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("chebi", "http://purl.org/obo/owl/CHEBI#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("envo","http://purl.org/obo/owl/ENVO#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("ncbitax","http://purl.org/obo/owl/NCBITaxon#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("pro","http://purl.org/obo/owl/PRO#");	
		owlModel.getGraph().getPrefixMapping().setNsPrefix("caro","http://purl.org/obo/owl/CARO#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("so","http://purl.org/obo/owl/SO#");
		owlModel.getGraph().getPrefixMapping().setNsPrefix("go","http://purl.org/obo/owl/GO#");

		owlModel.getGraph().getPrefixMapping().setNsPrefix("iao", "http://purl.obofoundry.org/obo/iao/2009-01-23/iao.owl");
		//specific case: the empty string means default namespace
		owlModel.getGraph().getPrefixMapping().setNsPrefix("",OBINs);

		//we remove the branch specific namespaces declaration
		owlModel.removeNsPrefix("obi_func");   	
		owlModel.removeNsPrefix("obi_biomat");
		owlModel.removeNsPrefix("obi_denrie");
		owlModel.removeNsPrefix("obi_obsolete");
		owlModel.removeNsPrefix("obi_rest");
		owlModel.removeNsPrefix("obi_rel");
		owlModel.removeNsPrefix("obi_plan");
		owlModel.removeNsPrefix("obi_annot");
		owlModel.removeNsPrefix("obi_data_trans");
		owlModel.removeNsPrefix("obi_quality");
		owlModel.removeNsPrefix("obi_role");
		owlModel.removeNsPrefix("obi_instr");
		owlModel.removeNsPrefix("obi_ext");
		owlModel.removeNsPrefix("obi_extd");
		owlModel.removeNsPrefix("obi_owlfull");


		//some clean-up
		//e.g. remove the defined classes buckets at the top
		//remove _defined protocol application (OBI_0600065)
		OntResource r = owlModel.getOntResource(OBINs + "OBI_0600065");
		//remove _defined_material (OBI_0000233)
		OntResource r2 = owlModel.getOntResource(OBINs + "OBI_0000233");

		//remove _defined_output (OBI_0000449)
		OntResource r3 = owlModel.getOntResource(OBINs + "OBI_0000449");
		
		
		//remove _realizable_entity (OBI_0000683)
		OntResource r4 = owlModel.getOntResource(OBINs + "OBI_0000683");

		r.remove();
		r2.remove();
		r3.remove();
		r4.remove();


		return owlModel;
	}
	/**
	 * The method merging the files
	 * @param newFilePath - the path to the file to be created
	 * @param physicalURI - the physical URI where to find the OBI branch files
	 */
	public static void mergeFiles(String newFilePath,String physicalURI, boolean ProtegeFriendly){

		//the output file
		File newFile = new File(newFilePath);

		//delete the file if it already exists  - otherwise we could run into trouble :-)
		if (newFile.exists())	{
			newFile.delete();
		}

		OntModel owlModel = buildMergedFiles(physicalURI,ProtegeFriendly);
		//writes the file
		try {
			writeFile(newFile,owlModel);
		}
		catch (Exception ex) {
			System.out.println("Unable to write the file");
			ex.printStackTrace();
			System.exit(1);
		}



	}



	//For testing purposes
/*
	public final static void main(String[] args) throws Exception  {

		//the physical URI of the files
		//String physicalURI = "/Users/mcourtot/Desktop/releaseTest/20090731/build/newids/";
		//String physicalURI = "/Users/mcourtot/Desktop/OBI/SVN/obi/trunk/src/ontology/branches/";
		String physicalURI = "/Users/mcourtot/Desktop/OBI/SVN/obi/releases/2009-07-31/branches/";
		

		/* We use to do that for the normal file, which is using the full import syntax, otherwise pellet was recognizing OBI as being OWL full, due to a discrepancy in the URI and the physical location of OBO_REL (error was untyped resource)
		 * now that we replaced the import OBO_REL by relationship, this is not the case anymore, and it gets recognized as an ontology
		 * we can therefore use the "short" syntax for the imports, which is the only one properly recognized by Protege 3 for identification of the namespaces.
		 * Dated release of end of december 2008, the OBI-ProtegeFriendly file becomes the OBI file.
		 */
//		String newFilePath = "/Users/mcourtot/Desktop/OBIJie.owl";
		//we check validity of the non-protege friendly version (last argument=false)
//		mergeFiles(newFilePath,physicalURI,false);


		//check consistency -> this is now done via the owlapi
		//	boolean valid = checkConsistency(newFilePath);
		///boolean valid = checkConsistency("/Users/mcourtot/Desktop/OBI/SVN/obi/trunk/src/ontology/branches/obi.owl");
		//	System.out.println("to be committed: "+ valid);


		
		//String newFilePath = "/Users/mcourtot/Desktop/OBI/SVN/obi/releases/2009-01-28/merged/pellet/OBIDI.owl";
		
		//String newFilePath = "/Users/mcourtot/Desktop/OBIJieNew.owl";

		
		//we check validity of the non-protege friendly version (last argument=false)
		//mergeFiles(newFilePath,physicalURI,false);

	//String newFilePathProtegeFriendly = "/Users/mcourtot/Desktop/OBI.owl";
		
		
	//	mergeFiles(newFilePathProtegeFriendly,physicalURI,false);

		//check consistency -> this is now done via the owlapi
		//boolean valid = checkConsistency(newFilePathProtegeFriendly);
		
		

//		System.out.println("to be committed: ");

	
	//System.out.println("to be committed: "+ valid);


//}



}
