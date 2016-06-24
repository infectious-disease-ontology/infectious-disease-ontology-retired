use strict;

# Combined the branches described in obi.owl into a single file
# Creates two versions: obi-all-in-one.owl and obi-all-in-one-dl.owl, the latter skipping any OWL-Full parts.

# grab the BFO entities. Could script this but 
my $entities = <<EOF
<!DOCTYPE rdf:RDF [
<!ENTITY base "http://www.ifomis.org/bfo/1.1">
<!ENTITY bfo "&base;#">
<!ENTITY snap "&base;/snap#">
<!ENTITY span "&base;/span#">
<!ENTITY dc "http://purl.org/dc/elements/1.1/">
<!ENTITY protege-dc-import "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl">
<!ENTITY protege-dc "&protege-dc-import;#">
<!ENTITY w3 "http://www.w3.org">
<!ENTITY owl "&w3;/2002/07/owl#">
<!ENTITY rdf "&w3;/1999/02/22-rdf-syntax-ns#">
<!ENTITY rdfs "&w3;/2000/01/rdf-schema#">
<!ENTITY xsd "&w3;/2001/XMLSchema#">
]>
EOF
    ;

my $obostuff = <<EOF
<!--oboInOwl meta-model - we must declare this here to avoid falling into owl-full-->
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasAlternativeId"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasDate"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasVersion"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasDbXref"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasDefaultNamespace"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasOBONamespace"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasDefinition"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasExactSynonym"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasNarrowSynonym"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasBroadSynonym"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasSynonymType"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasSubset"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#hasURI"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#inSubset"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#savedBy"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#replacedBy"/>
  <owl:AnnotationProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#consider"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#DbXref"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#Definition"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#Subset"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#Synonym"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#SynonymType"/>
  <owl:Class rdf:about="http://www.geneontology.org/formats/oboInOwl#ObsoleteClass"/>
  <owl:ObjectProperty rdf:about="http://www.geneontology.org/formats/oboInOwl#ObsoleteProperty"/>
EOF
    ;

open TOP, "<obi-lsw.owl.template" or die("can't find obi-lsw.owl.template");
my @lines = <TOP>;
close TOP;

# extract the names of the branches
my @obiParts = grep {/rdf:about="(.*?)"/;$1} @lines;

my @obiParts = map { /([0-9A-Za-z._\/\-]*)\.owl.*/;$1} @obiParts;
shift @obiParts; # pop off OBI.owl
unshift @obiParts, "obi-owl-full";

# protege wants this in the ontology header
my $defaultLang = '<protege:defaultLanguage rdf:datatype="http://www.w3.org/2001/XMLSchema#string">en</protege:defaultLanguage>';

# $dest = pathname to write to
# $dl = 1 if you are writing the owl-dl version, otherwise 0 or absent

sub writeMerged
  { my ($dest,$dl) = @_;
    open OUT, ">$dest";
    
    # Copy the header from TheRest.owl, which has all the creators, etc.
    open REST, "<TheRest.owl" or die("couldn't open TheRest.owl");

    my $first = 1;
    foreach my $line (<REST>) {
      last if $line =~ /<\/owl:Ontology>/; # until he end of the <ontology> section

      # include the namespaces used for the properties.
      if ($line =~ /xml:base=/) {
	$line =~ s/OBI\/TheRest.owl/OBI.owl/;
	print OUT '    xmlns:obi="http://obi.sourceforge.net/ontology/OBI.owl#"',"\n";
	print OUT '    xmlns:OBI="http://obi.sourceforge.net/ontology/OBI.owl#"',"\n";
	print OUT '    xmlns:j.0="http://obi.sourceforge.net/ontology/OBI.owl#"',"\n";
      }
      print OUT $line;
      if ($first) { print OUT $entities; $first = 0}
    }
    close REST;

    # add the default language attribute
    print OUT "  $defaultLang\n";

    # OK Done with the ontology section
    print OUT "  </owl:Ontology>\n";

    print OUT $obostuff;
    # Now include the bodies of the each of the branches
    foreach my $part ( @obiParts) {
      next if ($dl && $part=~/owl-full/); # exclude any file with owl-full in its name, if we are doing the DL version
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copy;
      while (<PART>) {
	if (/<\/owl:Ontology>/) { $copy = 1; next } # don't start copying until after the <ontology> section
	if (/<owl:Ontology rdf:about=".*?"\/>/) { $copy = 1; next }

	if (/<\/rdf:RDF>/) { $copy = 0 } # but don't copy the closing tag
	if ($copy) { print OUT $_ }
      }
      close PART;
    }
    print OUT "\n</rdf:RDF>\n"; # now close it all up
    close OUT;
  }


# write one version that is everything
writeMerged("obi-all-in-one.owl");
# and another that skips the OWL-Full parts.
writeMerged("obi-all-in-one-dl.owl",1);

# If we're running in the debugger, stop here to check things out.
$DB::single=1;1;

