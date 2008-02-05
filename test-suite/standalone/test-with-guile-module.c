#include <pthread.h>
#include <libguile.h>

void * thread_inner_main (void * unused);
void * thread_main (void * unused);
void * do_join (void * data);
void * inner_main (void * unused);

void * thread_inner_main (void * unused)
{
  int argc = 3;
  char* argv[] = { "guile",
		   "-c",
		   "(or (current-module) (exit -1))",
		   0 };
  scm_shell (argc, argv);

  return NULL; /* dummy */
}

void * thread_main (void * unused)
{
  scm_with_guile (&thread_inner_main, NULL);

  return NULL; /* dummy */
}

void * do_join (void * data)
{
  pthread_t *thread = (pthread_t *)data;

  pthread_join (*thread, NULL);

  return NULL; /* dummy */
}

void * inner_main (void * unused)
{
  pthread_t thread;

  pthread_create (&thread, NULL, &thread_main, NULL);
  scm_without_guile (do_join, &thread);

  return NULL; /* dummy */
}

int main (int argc, char **argv)
{
  scm_with_guile (&inner_main, NULL);

  return 0;
}
