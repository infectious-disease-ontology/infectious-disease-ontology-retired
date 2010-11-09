use strict;

my $instance_p = 0;


my $child = $ARGV[0];
my $parent = $ARGV[1];
my $branchpath = $ARGV[2];

if ($ARGV[0] =~ /^instance$/)
  { $instance_p =1;
    $child = $ARGV[1];
    $parent = $ARGV[2];
    $branchpath = $ARGV[3];
  }

if (!$branchpath)
{ $branchpath = `cd  \`dirname $0\`/../ontology/ido-core/; pwd` ;
  chomp $branchpath;
  $branchpath .= "/ido-external.owl";
}

# format: id prefix, uri prefix, source ontology, id pattern, example id (use a verified real one)
# add another line for each new ontology

my $externals_table = 
    [["PATO", "http://purl.org/obo/owl/PATO#PATO_", "http://purl.org/obo/owl/PATO", "\\d{7}","PATO:0001879"],
     ["CARO", "http://purl.org/obo/owl/CARO#CARO_", "http://purl.org/obo/owl/CARO", "\\d{7}", "CARO:0000000"],
     ["TRANS", "http://purl.org/obo/owl/TRANS#TRANS_", "http://purl.org/obo/owl/TRANS", "\\d{7}", "TRANS:0000000"],
     ["OGMS", "http://purl.obolibrary.org/obo/OGMS_", "http://purl.obolibrary.org/obo/ogms.owl", "\\d{7}", "OGMS:0000087"],
     ["OBI", "http://purl.obolibrary.org/obo/OBI_", "http://purl.obolibrary.org/obo/obi.owl", "\\d{7}", "OBI:0100026"],
     ["PRO", "http://purl.org/obo/owl/PRO#PRO_", "http://purl.org/obo/owl/PRO","\\d{9}","PRO:000001958"],
     ["FMA", "http://purl.org/obo/owl/FMA#FMA_", "http://purl.org/obo/owl/FMA","\\d+","FMA:7203"],
     ["CHEBI", "http://purl.org/obo/owl/CHEBI#CHEBI_", "http://purl.org/obo/owl/CHEBI","\\d+","CHEBI:16714"],
     ["GO", "http://purl.org/obo/owl/GO#GO_", "http://purl.org/obo/owl/GO","\\d{7}","GO:0047036"],
     ["CL", "http://purl.org/obo/owl/CL#CL_", "http://purl.org/obo/owl/CL","\\d{7}","CL:0000042"],
     ["NCBITaxon", "http://purl.org/obo/owl/NCBITaxon#NCBITaxon_", "http://purl.org/obo/owl/NCBITaxon","\\d+","NCBITaxon:2157"],
     ["ENVO", "http://purl.org/obo/owl/ENVO#ENVO_", "http://purl.org/obo/owl/ENVO","\\d{8}","ENVO:00000430"],
     ["RNAO", "http://purl.org/obo/owl/RNAO#RNAO_", "http://purl.org/obo/owl/RNAO","\\d{7}","RNAO:0000001"],
     ["SO", "http://purl.org/obo/owl/SO#SO_", "http://purl.org/obo/owl/SO","\\d{7}","SO:0000683"],
     ["UO", "http://purl.org/obo/owl/UO#UO_", "http://purl.org/obo/owl/UO","\\d{7}","UO:0000683"],
     ["UBERON", "http://purl.org/obo/owl/UBERON#UBERON_", "http://purl.org/obo/owl/UBERON","\\d{7}","UBERON:0000105"],
     ["VO", "http://purl.obolibrary.org/obo/VO_", "http://purl.obolibrary.org/obo/vo.owl","\\d{7}","VO:0000867"],
     ["snap", "http://www.ifomis.org/bfo/1.1/snap#", "http://www.ifomis.org/bfo/1.1","\\S+","snap:MaterialEntity"],
     ["span", "http://www.ifomis.org/bfo/1.1/span#", "http://www.ifomis.org/bfo/1.1","\\S+","span:Process"],
     ["NIF-GrossAnatomy", "http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#",
      "http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl","birnlex_\\d+","NIF-GrossAnatomy:birnlex_1373"]];

sub usage 
  { if (@_) {print "We're sorry, but there seems to be a problem: @_\n\n"};
  print "Usage:\nperl add-to-external.pl [\"instance\"] child parent [path to external.owl]\ne.g. perl add-to-external.pl PRO:000000001 CHEBI:23091. The default path to external is $branchpath\n";
  print "The script recognizes the following prefixes:\n";
  map { print "  $_->[0] (e.g. $_->[4]) Ontology: $_->[2]\n" } @{$externals_table};
  print "Additionally, IDO ids in the form IDO_0600065 or IDO:0600065 or IAO ids of the form IAO_0000412 or IAO:0000412 can be used for parent terms\n";
  exit;
}

if (!@ARGV) {usage()};


sub findit 
  { my $term = @_[0];
    grep {my $re1=$_->[0].":".$_->[3];my $re2=$_->[1].$_->[3];($term=~/$re1/ || $term=~/$re2/)} @{$externals_table}
  }

findit($child) or usage("child term '$child' doesn't look like a valid id");

($parent =~ /(IDO)(:|_)\d+/ || findit($parent)) or usage("parent term '$parent' doesn't look like a valid id.");

-e $branchpath or usage("$branchpath doesn't exist");

my ($parenturi,$childuri);

if ($child =~ /(.*?):(.*)$/)
{ my ($ont,$id) = ($1,$2); 
  my @found = findit($child);
  $childuri= @found[0]->[1].$id;
}
else { $childuri =~ $child; }
$DB::single=1;
if ($parent =~ /^((IDO)(:|_)\d+)$/)
{ my $id = $1; $id =~ s/:/_/;
  $parenturi = "http://purl.obolibrary.org/obo/$id" }
elsif ($parent =~ /^http/)
{ $parenturi = $parent }
elsif( $parent =~ /(.*?):(.*)$/)
{ my ($ont,$id) = ($1,$2);
  my @found = findit($parent);
  $parenturi= @found[0]->[1].$id;
}

(($parent=~ /^(IDO)(_|:)/) || ($parent=~ /^(snap|span):/) || `grep 'rdf:about="$parenturi">' $branchpath`) 
    or usage("$parenturi not present - parent terms must be present either be from IDO or IAO or in the external file before they are used as parents. Please use this script to add it then try again.\n");

`grep '<owl:Class rdf:about="$childuri">' $branchpath` and usage("$childuri already present, so not adding it again");

my $template;

if ($instance_p) 
  {  $template =<<EOF
  <rdf:Description rdf:about="_CHILD_">
    <rdf:type rdf:resource="_PARENT_"/>
    <IAO_0000412 rdf:resource="_ONT_"/>
  </rdf:Description>
EOF
   }
else
  {
 $template =<<EOF
  <owl:Class rdf:about="_CHILD_">
    <rdfs:subClassOf rdf:resource="_PARENT_"/>
    <IAO_0000412 rdf:resource="_ONT_"/>
  </owl:Class>
EOF
};

my @found = findit($child);
my $onturi = @found[0]->[2];

$template =~ s/_ONT_/$onturi/e;
$template =~ s/_CHILD_/$childuri/e;
$template =~ s/_PARENT_/$parenturi/e;

print "child: $childuri\n"."parent: $parenturi\n"."ontology: $onturi\n";
print "adding to $branchpath:\n$template\n";

open EXTERNAL, "<$branchpath" or usage("can't open $branchpath to read");
my @lines = <EXTERNAL>;
close EXTERNAL;
open EXTERNAL, ">$branchpath" or usage("can't open $branchpath to write");
foreach (@lines)
{ if (/<\/rdf:RDF>/)
  { print EXTERNAL $template,"$_\n" }
  else 
  { print EXTERNAL $_}
}
close EXTERNAL;


