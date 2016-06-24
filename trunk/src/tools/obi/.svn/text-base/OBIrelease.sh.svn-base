#!/bin/sh

#starting configuration: create a directory containing the OBIrelease.sh script (e.g. obireleases)

#the path to the LSW directory
####################################UPDATE THIS WITH THE LOCATION OF YOUR INSTALLATION ####################################
LSW_TRUNK_PATH=/Users/mcourtot/Desktop/releaseTest/svn-lsw/trunk
LSW_PATH=${LSW_TRUNK_PATH}/abcl
LSW_STARTUP_PATH=${LSW_TRUNK_PATH}/scripts


#############b set the ABCL_WD variable based on the above
export ABCL_WD=/Users/mcourtot/Desktop/releaseTest/svn-lsw/trunk/

######################################################## CHECKOUT THE TOOLS ###############################################
#you need to have the OBITools.jar in your classpath and to download lsw
# let's get them from SVN and manage that for you


#get lsw
svn co http://mumble.net:8080/svn/lsw/ ./svn-lsw/
#get OBITools.jar
svn co https://obi.svn.sourceforge.net/svnroot/obi/trunk/src/tools/build/OBITools 
#get binaries
svn co https://obi.svn.sourceforge.net/svnroot/obi/trunk/bin


#######################################################################################################################################

############################################################## BASIC SETUP ########################################################

SOMETHING_ALAN_CAN_GREP_FOR="Relevant for OBI Release:"


#the file where to write the merge
NEWFILEPATH="UncheckedMerge.owl"
#the file where to write the merge protege-friendly
NEWFILEPATHPROTEGE="UncheckedMergePROTEGE.owl"
#the path to this directory
HERE=`pwd`




#set up the classpath for the jar 
export CLASSPATH=$HERE:$HERE/OBITools/OBITools.jar
echo $SOMETHING_ALAN_CAN_GREP_FOR $CLASSPATH

#we create a directory for this release - its name is the date
CUR_DATE=`date +%Y%m%d`
mkdir $CUR_DATE


#let's go there and do some work!
cd $CUR_DATE

############################################################# create the structure for the files

#the path to the branch files
OBI_DIR_PATH=`pwd`/src/ontology/branches
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI DIR PATH is set to" $OBI_DIR_PATH 

#the path to the external files (bfo etc)
EXTERNAL_DIR_PATH=`pwd`/src/ontology/external
echo $SOMETHING_ALAN_CAN_GREP_FOR "EXTERNAL DIR PATH is set to" $EXTERNAL_DIR_PATH 

#the path to the instances files 
INSTANCES_DIR_PATH=`pwd`/src/ontology/branches/instances
echo $SOMETHING_ALAN_CAN_GREP_FOR "INSTANCES DIR PATH is set to" $INSTANCES_DIR_PATH 


#the path to the tools
OBI_TOOLS_PATH=`pwd`/src/tools
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI TOOLS PATH is set to" $OBI_TOOLS_PATH 

#the path to the code
OBI_CODE_PATH=`pwd`/src/tools/build
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI CODE PATH is set to" $OBI_CODE_PATH 

#the path to the build directory: will contain new branch files
OBI_BUILD_PATH=`pwd`/build
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI BUILD PATH is set to" $OBI_BUILD_PATH 
#create the build directory 
mkdir $OBI_BUILD_PATH

#the path to the merged directory: will contain release files (merge, doc...)
OBI_MERGED_PATH=`pwd`/merged
mkdir $OBI_MERGED_PATH
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI_MERGED_PATH is set to" $OBI_MERGED_PATH 
#the path to the subdirectory of the merged directory that will contain the protege specific files
OBI_MERGED_PATH_PROTEGE=`pwd`/merged/protege
mkdir $OBI_MERGED_PATH_PROTEGE
echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI_MERGED_PATH_PROTEGE is set to" $OBI_MERGED_PATH_PROTEGE 

# we do a fresh svn checkout
# we need only the branches and external files
# we are not taking spreadsheets and others
svn co  http://obi.svn.sourceforge.net/svnroot/obi/trunk/src/ontology/branches/ ./src/ontology/branches/

#TEMP HACK ################################# BEURK ############
cp $INSTANCES_DIR_PATH/dataTransformationInstances.owl $OBI_DIR_PATH
cp $INSTANCES_DIR_PATH/softwareInstances.owl $OBI_DIR_PATH
cp $INSTANCES_DIR_PATH/organizationInstances.owl $OBI_DIR_PATH
echo $SOMETHING_ALAN_CAN_GREP_FOR "instances files ugly temp hack BEURKKKK"




svn co  http://obi.svn.sourceforge.net/svnroot/obi/trunk/src/ontology/external/ ./src/ontology/external/
# we need the sourcecodes as well 

svn co  http://obi.svn.sourceforge.net/svnroot/obi/trunk/src/tools/build/ ./src/tools/build/
echo $SOMETHING_ALAN_CAN_GREP_FOR "SVN checked out"


#javac the client
javac $OBI_CODE_PATH/OBIReleaseClient.java 
echo $SOMETHING_ALAN_CAN_GREP_FOR "java client compiled"





#we need to copy obi.owl.template to obi.owl
#needed by modify-uris.pl
cp $OBI_DIR_PATH/obi.owl.template $OBI_DIR_PATH/obi.owl
echo $SOMETHING_ALAN_CAN_GREP_FOR "obi.owl.template copied to obi.owl"


echo $SOMETHING_ALAN_CAN_GREP_FOR "<?xml version=\"1.0\"?>
<rdf:RDF
    xmlns=\"http://purl.obolibrary.org/obo/\"
    xmlns:protege=\"http://protege.stanford.edu/plugins/owl/protege#\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
    xml:base=\"file://$OBI_DIR_PATH/\">
  <owl:Ontology rdf:about=\"http://purl.obolibrary.org/obo/\">
    <owl:imports> <owl:Ontology  rdf:about=\"AnnotationProperty.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataTransformation.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DigitalEntityPlus.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Biomaterial.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataFormatSpecification.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/bfo11.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro_bfo_bridge11.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Role.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"InstrumentAndPart.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"PlanAndPlannedProcess.owl\"/></owl:imports>    
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege-dc.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Relations.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"OBI-Function.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Quality.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"external.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"externalDerived.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Obsolete.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"external-byhand.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"obi-quick-id.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/IAO.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/ontology-metadata.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/obsolete.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"dataTransformationInstances.owl\"/></owl:imports> 
    <owl:imports> <owl:Ontology  rdf:about=\"softwareInstances.owl\"/></owl:imports> 
       <owl:imports> <owl:Ontology  rdf:about=\"organizationInstances.owl\"/></owl:imports>
    <protege:defaultLanguage rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">en</protege:defaultLanguage>

  </owl:Ontology>
</rdf:RDF>" > ./src/ontology/branches/obil.owl

echo $SOMETHING_ALAN_CAN_GREP_FOR "obil.owl created"

#we need the initial obi branches to modify their URIs and IDs
OBI_OWL_PATH=${OBI_DIR_PATH}/obil.owl


###################################################################################################################################

############################################################### URI-REPORT #########################################################

#the following command launch abcl, load the necessary lisp scripts, and execute the function to produce the uri-report.txt file
perl ${LSW_PATH} $*   --load ${LSW_STARTUP_PATH}/system-registry.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(list-obi-uris \"${OBI_BUILD_PATH}/uri-report.txt\" (load-kb-jena \"${OBI_OWL_PATH}\"))" --eval "(quit)"

echo $SOMETHING_ALAN_CAN_GREP_FOR "uri-report.txt created"

echo $SOMETHING_ALAN_CAN_GREP_FOR `pwd`

perl  ./src/tools/build/modify-uris.pl




################################################## INSTANCES REWRITE #######################################################


################################################# dataTransformationInstances
perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(rewrite-instance-file \"$OBI_BUILD_PATH/uri-rewrites.txt\" \"${OBI_BUILD_PATH}/newids/dataTransformationInstances.owl\" \"${OBI_BUILD_PATH}/newids/dataTransformationInstances.owl\" \"http://purl.obolibrary.org/obo/obi/dataTransformationInstances.owl\")"  --eval "(quit)"
echo $SOMETHING_ALAN_CAN_GREP_FOR "dataTransformationInstances.owl updated at $OBI_BUILD_PATH/newids/dataTransformationInstances.owl"

################################################# softwareInstances
perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(rewrite-instance-file \"$OBI_BUILD_PATH/uri-rewrites.txt\" \"${OBI_BUILD_PATH}/newids/softwareInstances.owl\" \"${OBI_BUILD_PATH}/newids/softwareInstances.owl\" \"http://purl.obolibrary.org/obo/obi/softwareInstances.owl\")"  --eval "(quit)"
echo $SOMETHING_ALAN_CAN_GREP_FOR "softwareInstances.owl updated at $OBI_BUILD_PATH/newids/softwareInstances.owl"

################################################# organizationInstances
perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(rewrite-instance-file \"$OBI_BUILD_PATH/uri-rewrites.txt\" \"${OBI_BUILD_PATH}/newids/organizationInstances.owl\" \"${OBI_BUILD_PATH}/newids/organizationInstances.owl\" \"http://purl.obolibrary.org/obo/obi/organizationInstances.owl\")"  --eval "(quit)"
echo $SOMETHING_ALAN_CAN_GREP_FOR "organizationInstances.owl updated at $OBI_BUILD_PATH/newids/organizationInstances.owl"





################################################## OPTIONAL - NEWIDS CHECK #######################################################

#files needed to open the newids files in protege
#not required -but I like checking that the files I will commit will be ok for others
cp $OBI_DIR_PATH/obi.owl $OBI_BUILD_PATH/newids/obi.owl


cp $OBI_DIR_PATH/obi.repository.template $OBI_BUILD_PATH/newids/obi.repository
mkdir $OBI_BUILD_PATH/external/
echo $SOMETHING_ALAN_CAN_GREP_FOR "created $OBI_CODE_PATH/external/ directory"
cp  $EXTERNAL_DIR_PATH/* $OBI_BUILD_PATH/external/
echo $SOMETHING_ALAN_CAN_GREP_FOR "CHECKPOINT: you can check in Protege the newly created files: open $OBI_BUILD_PATH/newids/obil.owl"
#at this point you should be able to open $OBI_BUILD_PATH/newids/obi.owl in protege (if you are under *nix system - otherwise you need the obi.repository.template.pc file and to modify it)
###################################################################################################################################


################################################## DATE AND REVISION NUMBER #######################################################
#note: we need to replace the date in TheRest.owl to do so, or we will get a consistency error from the reasoner

#we replace the date and the version number in TheRest.owl
TODAY=`date "+%G-%m-%d"`

perl -pi -e "s/<dc:date rdf:datatype=\"http:\/\/www.w3.org\/2001\/XMLSchema#date\">(.*)<\/dc:date>/<dc:date rdf:datatype=\"http:\/\/www.w3.org\/2001\/XMLSchema#date\">$TODAY<\/dc:date>/" $OBI_BUILD_PATH/newids/obi.owl
echo $SOMETHING_ALAN_CAN_GREP_FOR "date replaced in $OBI_BUILD_PATH/newids/obi.owl"

############################################## ugly path hack - for whatever reason the get-revision-number.sh script doesn't like to be called at the root (maybe some problem with external...?)
cd $OBI_CODE_PATH
SVN_REVISION_NUMBER=`./get-svn-revision.sh`
echo $SOMETHING_ALAN_CAN_GREP_FOR "got SVN revision number" $SVN_REVISION_NUMBER
cd $HERE

perl -pi -e "s/<owl:versionInfo xml:lang=\"en\">(.*)<\/owl:versionInfo>/<owl:versionInfo xml:lang=\"en\">1.0.$SVN_REVISION_NUMBER<\/owl:versionInfo>/" $OBI_BUILD_PATH/newids/obi.owl
echo $SOMETHING_ALAN_CAN_GREP_FOR "revision number replaced in $OBI_BUILD_PATH/newids/obi.owl"
###################################################################################################################################


########################################################## DISJOINTS, PURLS, ASSUMED INDIVIDUALS AND INFERRED SUPERCLASSES ###############################################################
echo $SOMETHING_ALAN_CAN_GREP_FOR "<?xml version=\"1.0\"?>
<rdf:RDF
    xmlns=\"http://purl.obolibrary.org/obo/\"
    xmlns:protege=\"http://protege.stanford.edu/plugins/owl/protege#\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
    xml:base=\"file://$OBI_BUILD_PATH/newids/\">
  <owl:Ontology rdf:about=\"http://purl.obolibrary.org/obo/\">
   <owl:imports> <owl:Ontology  rdf:about=\"AnnotationProperty.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataTransformation.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DigitalEntityPlus.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Biomaterial.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataFormatSpecification.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/bfo11.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro_bfo_bridge11.owl\"/></owl:imports>
<owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/IAO.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/ontology-metadata.owl\"/></owl:imports>
        <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/obsolete.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Role.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"InstrumentAndPart.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"PlanAndPlannedProcess.owl\"/></owl:imports>    
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege-dc.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Relations.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"OBI-Function.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Quality.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"external.owl\"/></owl:imports>
        <owl:imports> <owl:Ontology  rdf:about=\"obi-quick-id.owl\"/></owl:imports>
  <owl:imports> <owl:Ontology  rdf:about=\"externalDerived.owl\"/></owl:imports>
      <owl:imports> <owl:Ontology  rdf:about=\"Obsolete.owl\"/></owl:imports>
  <owl:imports> <owl:Ontology  rdf:about=\"external-byhand.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"dataTransformationInstances.owl\"/></owl:imports> 
    <owl:imports> <owl:Ontology  rdf:about=\"softwareInstances.owl\"/></owl:imports> 
       <owl:imports> <owl:Ontology  rdf:about=\"organizationInstances.owl\"/></owl:imports>
       
  </owl:Ontology>
</rdf:RDF>" > $OBI_BUILD_PATH/newids/obil.owl

OBI_OWL_PATH_NEW=$OBI_BUILD_PATH/newids/obil.owl
echo $SOMETHING_ALAN_CAN_GREP_FOR "new OWL path set to " $OBI_OWL_PATH_NEW





#the following command launches abcl, load the necessary lisp scripts, and execute the function to produce the disjoints.owl file
perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(write-disjoints (load-kb-jena \"${OBI_OWL_PATH_NEW}\") \"${OBI_BUILD_PATH}/newids/disjoints.owl\")"  --eval "(quit)"
echo $SOMETHING_ALAN_CAN_GREP_FOR "disjoints.owl created at $OBI_BUILD_PATH/newids/disjoints.owl"



perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(write-purls (load-kb-jena \"${OBI_OWL_PATH_NEW}\") (load-kb-jena \"http://purl.obofoundry.org/obo/obi.owl\") \"${OBI_BUILD_PATH}/list-purls.xml\")" --eval "(quit)"

echo $SOMETHING_ALAN_CAN_GREP_FOR "list-purls created at $OBI_BUILD_PATH/list-purls.xml"

perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)"  --eval "(write-assumed-individuals (load-kb-jena \"${OBI_BUILD_PATH}/newids/obid.owl\") \"${OBI_BUILD_PATH}/newids/assumed-individuals.owl\")" --eval "(quit)"


echo $SOMETHING_ALAN_CAN_GREP_FOR "assumed-individuals created at $OBI_BUILD_PATH/newids/assumed-individuals.owl"



echo $SOMETHING_ALAN_CAN_GREP_FOR "<?xml version=\"1.0\"?>
<rdf:RDF
    xmlns=\"http://purl.obolibrary.org/obo/\"
    xmlns:protege=\"http://protege.stanford.edu/plugins/owl/protege#\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
    xml:base=\"file://$OBI_BUILD_PATH/newids/\">
  <owl:Ontology rdf:about=\"http://purl.obolibrary.org/obo/\">
   <owl:imports> <owl:Ontology  rdf:about=\"AnnotationProperty.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataTransformation.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DigitalEntityPlus.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Biomaterial.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"DataFormatSpecification.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/bfo11.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/ro_bfo_bridge11.owl\"/></owl:imports>
<owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/IAO.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/ontology-metadata.owl\"/></owl:imports>
        <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/obsolete.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Role.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"InstrumentAndPart.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"PlanAndPlannedProcess.owl\"/></owl:imports>    
    <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/protege-dc.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Relations.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"OBI-Function.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"Quality.owl\"/></owl:imports>
    <owl:imports> <owl:Ontology  rdf:about=\"external.owl\"/></owl:imports>
<owl:imports> <owl:Ontology  rdf:about=\"disjoints.owl\"/></owl:imports>
<owl:imports> <owl:Ontology  rdf:about=\"assumed-individuals.owl\"/></owl:imports>
 <owl:imports> <owl:Ontology  rdf:about=\"external-byhand.owl\"/></owl:imports>
  <owl:imports> <owl:Ontology  rdf:about=\"externalDerived.owl\"/></owl:imports>
      <owl:imports> <owl:Ontology  rdf:about=\"Obsolete.owl\"/></owl:imports> 
          <owl:imports> <owl:Ontology  rdf:about=\"dataTransformationInstances.owl\"/></owl:imports> 
    <owl:imports> <owl:Ontology  rdf:about=\"softwareInstances.owl\"/></owl:imports> 
       <owl:imports> <owl:Ontology  rdf:about=\"organizationInstances.owl\"/></owl:imports>
       

  </owl:Ontology>
</rdf:RDF>" > $OBI_BUILD_PATH/newids/obidi.owl



echo $SOMETHING_ALAN_CAN_GREP_FOR "obidi.owl created (includes assumed individuals)"


perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)"  --eval "(write-inferred-superclasses (load-kb-jena \"${OBI_BUILD_PATH}/newids/obidi.owl\") \"${OBI_BUILD_PATH}/newids/inferred-superclasses.owl\")" --eval "(quit)"


echo $SOMETHING_ALAN_CAN_GREP_FOR "inferred-superclasses.owl created at $OBI_BUILD_PATH/newids/inferred-superclasses.owl"



############################################################### QC-QUERIES #########################################################
# preforms some check on the file
# currently:
# --- is there any rdfs:Class (instead of owl:Class)
# --- list of terms missing a curation status instance
# --- list of terms with extra curation instance (only one allowed per term)
# --- list of (OBI) terms with non OBI IDs
# --- list of terms missing a label
# --- list of classes that are asserted under a defined class
# writes the result of the queries in the merged directory, in the file qc-queries-report.txt
# TODO: add lost-terms (as soon as I understand the arguments ;-) )

###########perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(rdfs-class-report (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))" --eval "(quit)" > $OBI_MERGED_PATH/qc-queries-report1.txt

###########perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)"  --eval "(missing-curation (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))"  --eval "(quit)" > $OBI_MERGED_PATH/qc-queries-report2.txt

###########perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(extra-curation-status-instances (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))" --eval "(quit)" > $OBI_MERGED_PATH/qc-queries-report3.txt

###########perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(untranslated-uris (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))"  --eval "(quit)" > $OBI_MERGED_PATH/qc-queries-report4.txt


###########perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(missing-label (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))" --eval "(asserted-subclass-of-defined-class (load-kb-jena \"${OBI_OWL_PATH_NEW}\"))" --eval "(quit)" > $OBI_MERGED_PATH/qc-queries-report5.txt

###################################################################################################################################



###################################################################################################################################
##### we need to create the new obid.owl which will include inferred-superclasses.owl
##### we can't create it before as inferred-superclasses don't exist yet

echo $SOMETHING_ALAN_CAN_GREP_FOR "<?xml version=\"1.0\"?>
<rdf:RDF
    xmlns=\"http://purl.obolibrary.org/obo/\"
    xmlns:protege=\"http://protege.stanford.edu/plugins/owl/protege#\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
    xml:base=\"file://$OBI_BUILD_PATH/newids/\">
  <owl:Ontology rdf:about=\"http://purl.obolibrary.org/obo/\">
     <owl:imports rdf:resource=\"AnnotationProperty.owl\"/>
     <owl:imports rdf:resource=\"DataTransformation.owl\"/>
    <owl:imports rdf:resource=\"DigitalEntityPlus.owl\"/>
     <owl:imports rdf:resource=\"Biomaterial.owl\"/>
     <owl:imports rdf:resource=\"DataFormatSpecification.owl\"/>
     <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/bfo11.owl\"/>
     <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/ro.owl\"/>
    <protege:defaultLanguage rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">en</protege:defaultLanguage>
     <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/ro_bfo_bridge11.owl\"/>
    <owl:imports rdf:resource=\"Role.owl\"/>
     <owl:imports rdf:resource=\"InstrumentAndPart.owl\"/>
     <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/protege.owl\"/>
     <owl:imports rdf:resource=\"PlanAndPlannedProcess.owl\"/>
     <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/protege-dc.owl\"/>
<owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/iao/IAO.owl\"/>
    <owl:imports rdf:resource=\"$EXTERNAL_DIR_PATH/iao/ontology-metadata.owl\"/>
        <owl:imports> <owl:Ontology  rdf:about=\"$EXTERNAL_DIR_PATH/iao/obsolete.owl\"/></owl:imports>
     <owl:imports rdf:resource=\"Relations.owl\"/>
     <owl:imports rdf:resource=\"OBI-Function.owl\"/>
     <owl:imports rdf:resource=\"Quality.owl\"/>
     <owl:imports rdf:resource=\"external.owl\"/>
     <owl:imports rdf:resource=\"externalDerived.owl\"/>
     <owl:imports rdf:resource=\"Obsolete.owl\"/>
<owl:imports rdf:resource=\"disjoints.owl\"/>
 <owl:imports rdf:resource=\"external-byhand.owl\"/>
 <owl:imports rdf:resource=\"inferred-superclasses.owl\"/>
  <owl:imports rdf:resource=\"dataTransformationInstances.owl\"/>
   <owl:imports rdf:resource=\"softwareInstances.owl\"/>
    <owl:imports rdf:resource=\"organizationInstances.owl\"/>


       
  </owl:Ontology>
</rdf:RDF>" > $OBI_BUILD_PATH/newids/obid.owl



echo $SOMETHING_ALAN_CAN_GREP_FOR "obid.owl created (includes disjoints and inferred superclasses)"


echo $SOMETHING_ALAN_CAN_GREP_FOR "CHECKPOINT: you can check in Protege the newly created files: open $OBI_BUILD_PATH/newids/obid.owl to see the inferred hierarchy including disjoints"

########################################################## LSW ONTOLOGY REPORT ###############################################################
#################### kept separated from other lisp calls as it is quite long and I often comment it out for testing :-) ) ###################

####################perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp --load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(write-ontology-report (load-kb-jena \"${OBI_BUILD_PATH}/newids/obid.owl\") :fname \"${OBI_BUILD_PATH}/obi-lsw-report.html\")" --eval "(quit)"

echo $SOMETHING_ALAN_CAN_GREP_FOR "html report created at $OBI_BUILD_PATH/obi-lsw-report.html"

###################################################################################################################################


########################################################## SVN logs ###############################################################
######### we are interested only in the logs of the branch files, nothing else 
cd $OBI_DIR_PATH
svn log > $OBI_MERGED_PATH/SVNlogs.txt
echo $SOMETHING_ALAN_CAN_GREP_FOR "SVN logs created at $OBI_MERGED_PATH/SVNlogs.txt "
cd $HERE
####################






###################################################################################################################################
#                                     ALL FILES READY AND MODIFIED FOR MERGE AT THIS POINT                                        #
###################################################################################################################################
#everything is ready - let's merge
# run the java script
# the arguments are first the path to the new file to be created
# and second the physical location of the OBI files to be merged.

# we go back to the parent directory
cd $HERE


 cp $OBI_CODE_PATH/OBIReleaseClient.class .





#launch the merger itself
#the jar needs to be in your classpath
#RESULT=`java -Xmx1024m OBIReleaseClient $NEWFILEPATH $NEWFILEPATHPROTEGE $OBI_BUILD_PATH/newids/`
RESULT=`java -Xmx2048m OBIReleaseClient $NEWFILEPATH $NEWFILEPATHPROTEGE $OBI_BUILD_PATH/newids/`
# print out the result
echo $SOMETHING_ALAN_CAN_GREP_FOR "result is $RESULT"

# everything is ok, files are merged and consistency is good
if [ "$RESULT" == "true" ]
then
        echo "ok to commit"
	rm $OBI_MERGED_PATH/OBI.owl
	echo "$OBI_MERGED_PATH/OBI.owl deleted"

	rm $OBI_MERGED_PATH_PROTEGE/OBI-ProtegeFriendly.owl
	echo "$OBI_MERGED_PATH_PROTEGE/OBI-ProtegeFriendly.owl deleted"

        wget http://purl.obofoundry.org/obo/obi/protege/obi.pprj -P $OBI_MERGED_PATH_PROTEGE/
        echo "got the pprj file too!"

	rm $OBI_MERGED_PATH/md5.txt
	echo "$OBI_MERGED_PATH/md5.txt deleted"

        cp $NEWFILEPATH $OBI_MERGED_PATH/OBI-nocomment.owl
	echo "$OBI_MERGED_PATH/OBI.owl created"


#add comments
perl ${LSW_PATH} $*  --load ${LSW_STARTUP_PATH}/lsw-startup.lisp ---load ${OBI_CODE_PATH}/obi.asd --eval "(asdf::oos 'asdf::load-op :obi)" --eval "(comment-ids-in-owl-file \"${OBI_MERGED_PATH}/OBI-nocomment.owl\" \"${OBI_MERGED_PATH}/OBI.owl\" (load-kb-jena \"${OBI_MERGED_PATH}/OBI-nocomment.owl\"))" --eval "(quit)"

echo $SOMETHING_ALAN_CAN_GREP_FOR "OBI.owl now includes xml comments"


        md5 $OBI_MERGED_PATH/OBI.owl >> $OBI_MERGED_PATH/md5.txt
	echo "$OBI_MERGED_PATH/md5.txt created"

        cp $NEWFILEPATHPROTEGE $OBI_MERGED_PATH_PROTEGE/OBI-ProtegeFriendly.owl
	echo "$OBI_MERGED_PATH_PROTEGE/OBI-ProtegeFriendly.owl created"
        
        cp $OBI_BUILD_PATH/list-purls.xml $OBI_MERGED_PATH/list-purls.xml
	echo "list-purls copied to $OBI_MERGED_PATH "
	rm $OBI_BUILD_PATH/list-purls.xml
	cp $OBI_BUILD_PATH/obi-lsw-report.html $OBI_MERGED_PATH/obi-lsw-report.html 
	echo "obi-lsw-report copied to $OBI_MERGED_PATH"
	rm $OBI_BUILD_PATH/obi-lsw-report.html

	#the merge went through - we should commit the newids files at this point
exit 1
fi

# there has been a problem and we don't commit
if [ "$RESULT" == "false" ]
then
        echo "bouhouhouhouhou"
exit 1
fi

########################################################   TODO    #############################################################

## #  qc-queries: TODO: add lost-terms (as soon as I understand the arguments ;-) ) => Done
## #  replace repetitive calls to lisp with perl obi-lisp-eval (as soon as I manage with the paths ;-) ) => Done

###################################################################################################################################



