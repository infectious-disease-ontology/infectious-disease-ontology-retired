#!/bin/sh

# this script is to be used at J-3 to check if everything will go through with the release


#we create a directory for this release
CUR_DATE=`date +%Y%m%d`
mkdir $CUR_DATE
cd $CUR_DATE

#the file where to write the merge
NEWFILEPATH="UncheckedMerge.owl"
#the file where to write the merge protege-friendly
NEWFILEPATHPROTEGE="UncheckedMergePROTEGE.owl"



# we do a fresh svn checkout
svn co  https://obi.svn.sourceforge.net/svnroot/obi/branchDevelopment/trunk/ .
#we need to copy obi.owl.template to obi.owl
cp ./obi.owl.template ./obi.owl


# run the java script - TO DO: specify path
# the arguments are first the path to the new file to be created
# and second the physical location of the OBI files to be merged.

# we go back to the parent directory
cd ..

#launch the merger itself
RESULT=`java OBIReleaseClient $NEWFILEPATH $NEWFILEPATHPROTEGE $CUR_DATE/`

# print out the result
echo "result is $RESULT"

# everything is ok, files are merged and consistency is good
if [ "$RESULT" == "true" ]
then
	echo "ok to commit"
        cp $NEWFILEPATH ./OBI.owl
        md5sum $NEWFILEPATH >> md5.txt
	cp $NEWFILEPATHPROTEGE ./OBI-ProtegeFriendly.owl
exit 1
fi

# there has been a problem and we don't commit
if [ "$RESULT" == "false" ]
then 
	echo "bouhouhouhouhou"
exit 1
fi
