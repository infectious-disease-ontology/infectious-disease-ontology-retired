use strict;

# Combined the branches described in obi.owl into a single file
# Creates two versions: obi-merged.owl and obi-merged-dl.owl, the latter skipping any OWL-Full parts.

open TOP, "<obi.owl" or die("can't find obi.owl");
my @lines = <TOP>;
close TOP;

# extract the names of the branches
my @obiParts = grep {/<owl:imports rdf:resource="http:\/\/obi.sourceforge.net\/ontology\/OBI\/(.*)\.owl"/;$1} @lines;
my @obiParts = map { s/.*\/([A-Za-z.-]*)\.owl.*/$1/;chomp $_ ; $_} @obiParts;

# extract the other import statements
my @otherImports = grep {/<owl:imports rdf:resource="(.*?)"/;my $it=$1; if ($_=~/obi\.sourceforge\.net/) {undef} else {$it}} @lines;

# protege wants this in the ontology header
my $defaultLang = '<protege:defaultLanguage rdf:datatype="http://www.w3.org/2001/XMLSchema#string">en</protege:defaultLanguage>';

# $dest = pathname to write to
# $dl = 1 if you are writing the owl-dl version, otherwise 0 or absent

sub writeMerged
  { my ($dest,$dl) = @_;
    open OUT, ">$dest";
    
    # Copy the header from TheRest.owl, which has all the creators, etc.
    open REST, "<TheRest.owl" or die("couldn't open TheRest.owl");

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
    }
    close REST;

    # add the default language attribute
    print OUT "  $defaultLang\n";

    # now add the imports. Fix the relation ontology links because OBO changed them, and add he <ontology> surrounding them
    # because their ontology name isn't the same as the URL. Doing so prevents pellet from complaining OWL-FULL
    foreach my $import (@otherImports) {
      if ($import =~ /\/ro/)
	{ # $import =~ s/\/ro_bfo_bridge/\/OBO_REL_BFO_BRIDGE/;
	    $import =~ s/purl.org\/obo\/owl\/ro_bfo1-1_bridge/obofoundry.org\/ro\/ro_bfo1-1_bridge.owl/;
	   $import =~ s/\/ro"/\/OBO_REL"/; 
         #comment the following line to fix a bug in protege 3 that avoid display of the ontology annotations if imports use this syntax
         #$import =~ s/<owl:imports rdf:resource(.*)/<owl:imports> <owl:Ontology rdf:about$1<\/owl:imports>/;
	}
      print OUT $import 
    }

    # OK Done with the ontology section
    print OUT "  </owl:Ontology>\n";

    # Now include the bodies of the each of the branches
    foreach my $part ( @obiParts) {
      next if ($dl && $part=~/owl-full/); # exclude any file with owl-full in its name, if we are doing the DL version
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copy;
      print OUT "<!-- start branch: ",$part," -->\n";
      while (<PART>) {
	if (/<\/owl:Ontology>/) { $copy = 1; next } # don't start copying until after the <ontology> section
	if (/<\/rdf:RDF>/) { $copy = 0 } # but don't copy the closing tag
	if ($copy) { print OUT $_ }
      }
      close PART;
      print OUT "<!-- end branch: ",$part," -->\n";
    }
    print OUT "\n</rdf:RDF>\n"; # now close it all up
    close OUT;
  }

# write one version that is everything
writeMerged("obi-merged.owl");
# and another that skips the OWL-Full parts.
writeMerged("obi-merged-dl.owl",1);

# If we're running in the debugger, stop here to check things out.
$DB::single=1;1;

