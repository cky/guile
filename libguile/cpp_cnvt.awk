# Converts a list of symbols into C expressions which define the symbols
# in Guile.
{
print "#ifdef " $0;
print "scm_c_define (\""$0"\", SCM_MAKINUM ("$0"));";
print "#endif"
}
