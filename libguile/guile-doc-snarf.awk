#!/usr/bin/awk -f
# Written by Greg J. Badros, <gjb@cs.washington.edu>
# 12-Dec-1999

BEGIN { FS="|"; 
        filename = ARGV[1]; ARGV[1] = ""; 
        dot_x_file = filename; dot_doc_file = filename;
	sub(/\..*$/,".x",dot_x_file);
	sub(/\..*$/,".doc",dot_doc_file); 
        # be sure to put something in the files to help make out
        printf "" > dot_x_file;  
        printf "" > dot_doc_file;
}

/^[ \t]*%%%/ { copy = $0; 
               gsub(/[ \t]*%%%/, "", copy); 
               gsub(/\$\$\$.*$/, "", copy); 
               print copy > dot_x_file  }

/\$\$\$/,/@@@/ { copy = $0; 
                 if (match(copy,/\$\$\$R/)) { registering = 1; } 
                 else {registering = 0; } 
                 gsub(/.*\$\$\$./,"", copy); 
                 gsub(/@@@.*/,"",copy); 
                 gsub(/[ \t]+/," ", copy); 
		 sub(/^[ \t]*/,"(", copy);
                 gsub(/\"/,"",copy); 
		 sub(/ \(/," ",copy);
		 numargs = gsub(/SCM /,"", copy);
		 numcommas = gsub(/,/,"", copy);
		 numactuals = $2 + $3 + $4;
		 location = $5;
		 gsub(/\"/,"",location);
		 sub(/^[ \t]*/,"",location);
		 sub(/[ \t]*$/,"",location);
		 sub(/: /,":",location);
		 gsub(/[ \t]*\|.*$/,"",copy);
		 sub(/ )/,")",copy);
		 if (numargs != numactuals && !registering) 
		   { print location ":*** `" copy "' is improperly registered as having " numactuals " arguments"; }
		 print "\n" copy (registering?")":"") > dot_doc_file ; }

/@@@/,/@!!!.*$/ { copy = $0; 
                      gsub(/.*@@@/,"",copy); 
		      gsub(/^[ \t]*"?/,"", copy);
		      gsub(/\"?[ \t]*@!!!.*$/,"", copy);
                      gsub(/\\\"/,"\"",copy);
                      gsub(/[ \t]*$/,"", copy);
                      if (copy != "") { print copy > dot_doc_file } }
/@!!![ \t]/ { print "[" location "]" >> dot_doc_file; }

