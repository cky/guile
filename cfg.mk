old_NEWS_hash = d41d8cd98f00b204e9800998ecf8427e
git-version-gen-tag-sed-script :=					\
  's/^release_\([0-9]\+\)-\([0-9]\+\)-\([0-9]\+\)/v\1.\2\.\3/g'

local-checks-to-skip :=				\
  sc_makefile_at_at_check			\
  sc_prohibit_HAVE_MBRTOWC			\
  sc_prohibit_empty_lines_at_EOF		\
  sc_prohibit_have_config_h			\
  sc_prohibit_safe_read_without_use		\
  sc_prohibit_stat_st_blocks
