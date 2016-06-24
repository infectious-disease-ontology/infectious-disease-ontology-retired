use strict;

open TOP, "<obi.owl" or die("can't find obi.owl");
my @lines = <TOP>;
close TOP;

my $namespaces = join ("",<DATA>);

my $copydir = "newns/";

# extract the names of the branches

my @obiParts = grep {/<owl:imports rdf:resource="http:\/\/obi.sourceforge.net\/ontology\/OBI\/(.*)\.owl"/;$1} @lines;
my @obiParts = map { s/.*\/([A-Za-z.-]*)\.owl.*/$1/;chomp $_ ; $_} @obiParts;

sub replaceNamespaces
  {

    foreach my $part ( @obiParts) {
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copypath = $copydir.$part.".owl";
      open PARTCOPY, ">$copypath" or die ("Trouble writing $path");
      my $rewrite=1;
      while (<PART>) {
	  if (/xml:base/)
	  { print PARTCOPY $namespaces;
	    $rewrite=0
	  }
	  elsif ($rewrite) { next if /^\s*xmlns/ || /^\s*$/ }
	  print PARTCOPY $_;
      }
      close PART;
      close PARTCOPY;
    }
  }

replaceNamespaces();

$DB::single=1;1;

__DATA__
   xmlns:obi_rest="http://purl.obolibrary.org/obo/obi/TheRest.owl#"
   xmlns:obi_obsolete="http://purl.obolibrary.org/obo/obi/Obsolete.owl#"
   xmlns:obi_func="http://purl.obolibrary.org/obo/obi/OBI-Function.owl#"
   xmlns:obi_biomat="http://purl.obolibrary.org/obo/obi/Biomaterial.owl#"
   xmlns:obi_denrie="http://purl.obolibrary.org/obo/obi/DigitalEntityPlus.owl#"
   xmlns:obi_rel="http://purl.obolibrary.org/obo/obi/Relations.owl#"
   xmlns:obi_plan="http://purl.obolibrary.org/obo/obi/PlanAndPlannedProcess.owl#"
   xmlns:obi_annot="http://purl.obolibrary.org/obo/obi/AnnotationProperty.owl#"
   xmlns:obi_data_trans="http://purl.obolibrary.org/obo/obi/DataTransformation.owl#"
   xmlns:obi_quality="http://purl.obolibrary.org/obo/obi/Quality.owl#"
   xmlns:obi_role="http://purl.obolibrary.org/obo/obi/Role.owl#"
   xmlns:obi_instr="http://purl.obolibrary.org/obo/obi/InstrumentAndPart.owl#"
   xmlns:obi_ext="http://purl.obolibrary.org/obo/obi/external.owl#"
   xmlns:obi_extd="http://purl.obolibrary.org/obo/obi/externalDerived.owl#"
   xmlns:obi_owlfull="http://purl.obolibrary.org/obo/obi/obi-owl-full.owl#"
   xmlns:ro="http://www.obofoundry.org/ro/ro.owl#"
   xmlns:rotoo="http://purl.org/obo/owl/ro#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
   xmlns:owl="http://www.w3.org/2002/07/owl#"
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:dcterms="http://purl.org/dc/terms/"
   xmlns:protege-dc="http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#"
   xmlns:bfo="http://www.ifomis.org/bfo/1.1#"
   xmlns:robfo="http://purl.org/obo/owl/ro_bfo_bridge/1.1#"
   xmlns:snap="http://www.ifomis.org/bfo/1.1/snap#"
   xmlns:span="http://www.ifomis.org/bfo/1.1/span#"
   xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
   xmlns:pato="http://purl.org/obo/owl/PATO#"
   xmlns:cell="http://purl.org/obo/owl/CL#"
   xmlns:protege="http://protege.stanford.edu/plugins/owl/protege#"
   xmlns:go="http://www.geneontology.org/formats/oboInOwl#"
   xmlns:obi="http://purl.obolibrary.org/obo/"
   xmlns="http://purl.obolibrary.org/obo/"
