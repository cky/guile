SCM_BENCHMARKS = benchmarks/0-reference.bm			\
	         benchmarks/continuations.bm                    \
                 benchmarks/if.bm                               \
                 benchmarks/logand.bm

SCM_BENCHMARKS_DIRS =

EXTRA_DIST = guile-benchmark lib.scm $(SCM_BENCHMARKS)

## Automake should be able to handle the distribution of benchmarks/dirfoo
## etc without any help, but not all version can handle 'deep'
## directories.  So we do it on our own.
dist-hook:
	for d in "$(SCM_BENCHMARKS_DIRS)"; do \
	  cp -pR $(srcdir)/$$d $(distdir)/$$d; \
        done
