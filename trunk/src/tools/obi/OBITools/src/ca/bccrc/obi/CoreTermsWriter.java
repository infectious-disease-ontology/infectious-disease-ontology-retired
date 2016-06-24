/**
 * 
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

import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.AnnotationProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.impl.StatementImpl;

import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.RDFS;


/**
 * @author mcourtot
 *
 */
public class CoreTermsWriter {

	private static String xmlbase = "http://purl.obofoundry.org/obo/";
	//the Ontology URI
	private static String obiURI = "http://purl.obofoundry.org/obo/obi.owl";

	private static List<String> listURIs = new ArrayList<String>();
	private static List<OntClass> listSupers = new ArrayList<OntClass>();
	private static List<OntClass> listSupers2 = new ArrayList<OntClass>();

	//declaration of the OBI namespace
	private static String OBINs = "http://purl.obofoundry.org/obo/";



	public static void addClassesToModel(List classes, OntModel newModel){

		for (Iterator<OntClass> it = classes.iterator(); it.hasNext(); ) {

			OntClass thisClass = it.next();

			ExtendedIterator iter3 = thisClass.listProperties();

			if (thisClass.getURI() != null)
			{
				OntClass newClass = newModel.createClass(thisClass.getURI());

				System.out.println("URI: "+ thisClass.getURI());

				if(thisClass.getProperty(RDFS.label)!=null){
					RDFNode label = thisClass.getProperty(RDFS.label).getObject();

					newClass.addProperty(RDFS.label, label);

				}
				//we may want to get equivalent classes
				//though the following doesn't work
				/*	if (thisClass.getEquivalentClass() != null) {

					ExtendedIterator iter4 = thisClass.getEquivalentClass().listProperties();
					// ExtendedIterator iter4 = thisClass.getEquivalentClass().listSuperClasses();
					if (thisClass.getEquivalentClass().isRestriction())
					{


						Restriction r = thisClass.getEquivalentClass().asRestriction();


						if (r.isAllValuesFromRestriction()) {
							AllValuesFromRestriction av = r.asAllValuesFromRestriction();
							newClass.addEquivalentClass(newModel.createSomeValuesFromRestriction(null, av.getOnProperty(), av.getAllValuesFrom()));
						}
						if (r.isSomeValuesFromRestriction()) {
							SomeValuesFromRestriction av = r.asSomeValuesFromRestriction();
							newClass.addEquivalentClass(newModel.createSomeValuesFromRestriction(null, av.getOnProperty(), av.getSomeValuesFrom()));

						}

					}


				}
				 */


				//write properties of the class
				while(iter3.hasNext()) {		
					Statement stmt = (Statement)iter3.next();
					//we get only the OBI/IAO properties (e.g. not rdf type etc)
					if (stmt.getPredicate().toString().matches("http://purl.obofoundry.org/obo/(.*)"))
					{
						Resource obins = newModel.createResource(obiURI);
						StatementImpl stmt2 = new StatementImpl(newClass,stmt.getPredicate(),stmt.getObject());
						newModel.add(stmt2);
					}

				}


				for (Iterator i = thisClass .listSuperClasses( true ); i.hasNext(); ) {
					OntClass c = (OntClass) i.next();

					if (c.isRestriction()) {	
						//if we want to get restrictions
						Restriction r = c.asRestriction();

						/*if (r.isAllValuesFromRestriction()) {
							AllValuesFromRestriction av = r.asAllValuesFromRestriction();
							newClass.addSuperClass(newModel.createAllValuesFromRestriction(null, av.getOnProperty(), av.getAllValuesFrom()));
						}
						if (r.isSomeValuesFromRestriction()) {
							SomeValuesFromRestriction av = r.asSomeValuesFromRestriction();
							newClass.addSuperClass(newModel.createSomeValuesFromRestriction(null, av.getOnProperty(), av.getSomeValuesFrom()));
						}
						 */

					}
					//just write superclasses
					else
						newClass.addSuperClass(c);
				}
			}




		}
	}



	public static void getSuperclasses(OntClass ClassRoot)  {
		//add initial class to the list
		listSupers.add(ClassRoot);
		try {
			//get its superclasses
			for (Iterator allClassTermsInModel = ClassRoot.listSuperClasses(true); allClassTermsInModel.hasNext();)  {
				OntClass Class = (OntClass) allClassTermsInModel.next();
				//for each of them go and get superclasses and add to list
				listSupers.add(Class);
				getSuperclasses(Class);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}


	}
	
	//write the file
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

	
	public static OntModel prepareModel(OntModel owlModelCore)	{
		//we also add the AnnotationProperty for the defaultLanguage of Protege, otherwise Pellet complains OWL full
		owlModelCore.createAnnotationProperty("http://protege.stanford.edu/plugins/owl/protege#defaultLanguage");

		
		//specific case: the empty string means default namespace
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("",OBINs);

		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("xsd", "http://www.w3.org/2001/XMLSchema#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");

		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("owl", "http://www.w3.org/2002/07/owl#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("daml", "http://www.daml.org/2001/03/daml+oil#");

		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("dcterms", "http://purl.org/dc/terms/");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("dc", "http://purl.org/dc/elements/1.1/");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("protege", "http://protege.stanford.edu/plugins/owl/protege#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("protege-dc", "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("oboInOwl", "http://www.geneontology.org/formats/oboInOwl#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("oborel","http://purl.org/obo/owl/OBO_REL#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("relationship","http://purl.org/obo/owl/relationship#");


		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("bfo", "http://www.ifomis.org/bfo/1.1#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("robfo", "http://purl.org/obo/owl/ro_bfo_bridge/1.1#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("snap", "http://www.ifomis.org/bfo/1.1/snap#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("span", "http://www.ifomis.org/bfo/1.1/span#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("ro", "http://www.obofoundry.org/ro/ro.owl#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("rotoo", "http://purl.org/obo/owl/ro#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("pato", "http://purl.org/obo/owl/PATO#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("cell", "http://purl.org/obo/owl/CL#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("chebi", "http://purl.org/obo/owl/CHEBI#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("envo","http://purl.org/obo/owl/ENVO#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("ncbitax","http://purl.org/obo/owl/NCBITaxon#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("pro","http://purl.org/obo/owl/PRO#");	
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("caro","http://purl.org/obo/owl/CARO#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("so","http://purl.org/obo/owl/SO#");
		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("go","http://purl.org/obo/owl/GO#");

		owlModelCore.getGraph().getPrefixMapping().setNsPrefix("iao", "http://purl.obofoundry.org/obo/iao/2009-01-23/iao.owl");
		return owlModelCore;
		
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		//hophophop!
		System.out.println("launching");

		//build the model using Pellet spec - we want ot get inferred superclasses
		OntModel owlModel = ModelFactory.createOntologyModel(PelletReasonerFactory.THE_SPEC);

		//load OBI
		owlModel.read( "file:///Users/mcourtot/Desktop/OBI/SVN/obi/trunk/src/ontology/branches/obi.owl" );
		
		//where to write the file
		String newFilePath = "/Users/mcourtot/Desktop/OBICore.owl";
		

		//get imports
		owlModel.loadImports();



		//test method - to check if we actually got OBI
		//get all classes in OBI 
		/*	ExtendedIterator classes = owlModel.listClasses();
		List<OntClass> listClasses = classes.toList();
		for (Iterator<OntClass> it = listClasses.iterator(); it.hasNext(); ) {

			OntClass thisClass = it.next();

			//only for OBI classes
			//int index = thisClass.getLocalName().indexOf("OBI");
			//int index = thisClass.getURI().indexOf("obofoundry");
			//System.out.println("URI: "+thisClass.getURI());
			if (thisClass.getURI() != null)// && thisClass.getURI().indexOf("obofoundry") != -1)
			{
				if(thisClass.hasProperty(inBranchAnnPropObj) && thisClass.hasProperty(RDFS.label) && thisClass.getProperty(RDFS.label).getObject().toString().equals("manufacturing objective@en"))
					// && thisClass.getProperty(RDFS.label).getObject().toString().equals("core^^http://www.w3.org/2001/XMLSchema#string"))
				{
					System.out.println("get superclass of "+thisClass.getProperty(RDFS.label).getObject().toString());
					getSuperclasses(thisClass);
				}
			}
		}*/


//list of OBI core terms
		//Biomaterial branch
		listURIs.add(OBINs+"IAO_0000018"); //material entity
		listURIs.add(OBINs+"OBI_0100026"); //organism
		listURIs.add(OBINs+"OBI_0000245"); //organization
		listURIs.add("http://purl.org/obo/owl/CHEBI#CHEBI_23367"); //molecular entities
		listURIs.add(OBINs+"OBI_0302729"); //chemical entities in solution
		listURIs.add(OBINs+"OBI_0000256"); //environmental matter
		listURIs.add(OBINs+"OBI_0100015"); //anatomical entity
		listURIs.add(OBINs+"OBI_0000047"); //processed material - was artifact object
		listURIs.add(OBINs+"OBI_0100051"); //specimen


		//DENRIE
		listURIs.add(OBINs+"IAO_0000027"); //data item
		listURIs.add(OBINs+"IAO_0000100"); //data set
		//data structure
		listURIs.add(OBINs+"IAO_0000091"); //narrative object - report
		listURIs.add(OBINs+"IAO_0000059"); //narrative object - figure
		listURIs.add(OBINs+"IAO_0000010"); //software
		//variable
		listURIs.add(OBINs+"OBI_0000074"); //hypothesis
		listURIs.add(OBINs+"IAO_0000088"); //report of results
		listURIs.add(OBINs+"IAO_0000035"); //conclusion
		listURIs.add(OBINs+"IAO_0000005"); //objective specification
		listURIs.add(OBINs+"OBI_0500000"); //study design
		listURIs.add(OBINs+"IAO_0000032"); //scalar measurement datum

		//Role
		listURIs.add(OBINs+"OBI_0000275"); //analyte role
		listURIs.add(OBINs+"OBI_0000587"); //protocol participant role
		listURIs.add(OBINs+"OBI_0000022"); //specified input role
		listURIs.add(OBINs+"OBI_0000657"); //specified output role
		listURIs.add(OBINs+"OBI_0000067"); //evaluant role
		listURIs.add(OBINs+"OBI_0000086"); //reagent role
		listURIs.add(OBINs+"OBI_0000133"); //reference role
		//study personnel role
		listURIs.add(OBINs+"OBI_0000097"); //study subject role
		listURIs.add(OBINs+"OBI_0000112"); //specimen role


		//PAPP
		listURIs.add(OBINs+"OBI_0000066"); //investigation
		listURIs.add(OBINs+"OBI_0500000"); //study design - already in from denrie
		listURIs.add(OBINs+"OBI_0000272"); //protocol
		listURIs.add(OBINs+"OBI_0000070"); //assay
		listURIs.add(OBINs+"OBI_0000443"); //analyte assay
		listURIs.add(OBINs+"OBI_0000274"); //adding a material entity into a target (was material administration?)
		listURIs.add(OBINs+"OBI_0000011"); //planned process
		listURIs.add(OBINs+"IAO_0000005"); //objective specification - already in from DENRIE
		listURIs.add(OBINs+"IAO_0000104"); //plan specification
		listURIs.add(OBINs+"OBI_0000094"); //processing material - was artifact creation?
		listURIs.add(OBINs+"OBI_0600014"); //material separation
		listURIs.add(OBINs+"OBI_0000457"); //manufacturing


		//DT
		listURIs.add(OBINs+"OBI_0200000"); //DT
		listURIs.add(OBINs+"OBI_0200166"); //DT objective
		//listURIs.add(OBINs+"OBI_0000417"); // achieves_planned_objective
		//listURIs.add(OBINs+"OBI_0000301"); //has_specified_output_information
		//listURIs.add(OBINs+"OBI_0000315"); //has_specified_input_information
		listURIs.add(OBINs+"IAO_0000027"); //data item - already in from DENRIE
		listURIs.add(OBINs+"IAO_0000100"); //data set - already in from DENRIE
		//report figure -  - already in from DENRIE
		//software -  - already in from DENRIE


		//instrument
		listURIs.add(OBINs+"OBI_0400002"); //device
		listURIs.add(OBINs+"OBI_0400003"); //instrument
		listURIs.add(OBINs+"OBI_0000050"); //platform
		listURIs.add(OBINs+"OBI_0000047"); //processed material - was artifact object - already in from Biom
		listURIs.add(OBINs+"OBI_0400167"); //device function
		listURIs.add(OBINs+"OBI_0000402"); //canonical realization of device function
		listURIs.add(OBINs+"OBI_0000453"); //produce data function
		listURIs.add(OBINs+"OBI_0000392"); //information processor function
		//span:Process
		listURIs.add(OBINs+"OBI_0000272"); //protocol - already in from PaPP

		//for each of these classes, go and fetch
		for (Iterator<String> it = listURIs.iterator(); it.hasNext(); ) {
			OntClass coreClass = owlModel.getOntClass(it.next());
			System.out.println("getting "+ coreClass.getURI());
			getSuperclasses(coreClass );
		}








		OntModelSpec spec2 = new OntModelSpec( OntModelSpec.OWL_DL_MEM );

		OntModel owlModelCore = ModelFactory.createOntologyModel( spec2, null );
		
		//add namespaces schmilblick
		owlModelCore = prepareModel(owlModelCore);

		Ontology ont = owlModelCore.createOntology(obiURI);
		
		//add imports
		//ont.addImport(owlModelCore.createOntology("http://purl.obofoundry.org/obo/iao/2009-01-23/iao.owl"));
		//ont.addImport(owlModelCore.createOntology("http://www.ifomis.org/bfo/1.1"));
		//ont.addImport(owlModelCore.createOntology("http://www.obofoundry.org/ro/ro.owl"));
		//ont.addImport(owlModelCore.createOntology("http://purl.org/obo/owl/ro_bfo_bridge/1.1"));
		
		//we need to add those imports for the protege display label and to get the annotation properties
		ont.addImport(owlModelCore.createOntology("http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl"));
		ont.addImport(owlModelCore.createOntology("http://purl.obofoundry.org/obo/iao/2009-01-23/ontology-metadata.owl"));

		//add results to the model
		addClassesToModel(listSupers,owlModelCore);

		//write the results

		File newFile = new File(newFilePath);
		writeFile(newFile,owlModelCore);
		System.out.println("file written at " + newFilePath);

	}

}
