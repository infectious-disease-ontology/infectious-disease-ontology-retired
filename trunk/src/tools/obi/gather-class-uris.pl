use strict;

open TOP, "<obi.owl" or die("can't find obi.owl");
my @lines = <TOP>;
close TOP;
my $include_existing =1;
my $copydir = "newids/";
my $newNamespace = "http://purl.org/obo/";

# extract the names of the branches
my @obiParts = grep {/<owl:imports rdf:resource="http:\/\/obi.sourceforge.net\/ontology\/OBI\/(.*)\.owl"/;$1} @lines;
my @obiParts = map { s/.*\/([A-Za-z.-]*)\.owl.*/$1/;chomp $_ ; $_} @obiParts;

# extract the other import statementsn
my @otherImports = grep {/<owl:imports rdf:resource="(.*?)"/;my $it=$1; if ($_=~/obi\.sourceforge\.net/) {undef} else {$it}} @lines;

my @replacements;
my %replacements;

sub findnames 
  {
    foreach my $part ( @obiParts) {
      next if ($part=~/Relation|AnnotationProperty|owl-full/); 
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copy;
      while (<PART>) {
	if (/<\/owl:Ontology>/) { $copy = 1; next } # don't start copying until after the <ontology> section
	if (/<\/rdf:RDF>/) { $copy = 0 } # but don't copy the closing tag
	if ($copy) 
	  { 
	    my @names = $_ =~ /rdf:(about|id|resource)="(.*?)"/g;
#	    my %them  = @names;
	    @replacements = (grep {/obi\.sourceforge/ && ($include_existing || !/OBI_/)} @names,@replacements);
#	    @replacements = (@names,@replacements);
	    print OUT $_ 
	  }
      }
      close PART;
    }
  }

sub replacenames
  {
    my $count=1;

    foreach my $name(@replacements)
      { if (!$replacements{$name})
	  { $replacements{$name}= sprintf("$newNamespace\OBI_%06d",$count++); }
      };

    foreach my $part ( @obiParts) {
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copypath = $copydir.$part.".owl";
      open PARTCOPY, ">$copypath" or die ("Trouble writing $path");
      my $copy;
      while (<PART>) {
	if (/<\/owl:Ontology>/) { print PARTCOPY $_; $copy = 1; next } # don't start copying until after the <ontology> section
	if (/<\/rdf:RDF>/) { $copy = 0 } # but don't copy the closing tag
	if ($copy) 
	  { 
	    s/rdf:(about|id|resource)="(.*?)"/"rdf:".$1."=\"".($replacements{$2}||$2)."\""/ge;
	    s/http:\/\/obi.sourceforge.net\/ontology\/OBI.owl#/$newNamespace/;
	  }
	print PARTCOPY $_; 
      }
      close PART;
      close PARTCOPY;
    }
  }

findnames();
replacenames();

$DB::single=1;1;

print join("\n",keys %replacements);
