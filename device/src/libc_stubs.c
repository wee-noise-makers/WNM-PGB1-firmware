#include <unistd.h>
#include <stdbool.h>
#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>

extern char __StackLimit; /* Set by linker.  */

void *_sbrk(int incr) {
    extern char end; /* Set by linker.  */
    static char *heap_end;
    char *prev_heap_end;

    if (heap_end == 0)
        heap_end = &end;

    prev_heap_end = heap_end;
    char *next_heap_end = heap_end + incr;

    if (__builtin_expect(next_heap_end > (&__StackLimit), false)) {
#if PICO_USE_OPTIMISTIC_SBRK
        if (heap_end == &__StackLimit) {
//        errno = ENOMEM;
            return (char *) -1;
        }
        next_heap_end = &__StackLimit;
#else
        return (char *) -1;
#endif
    }

    heap_end = next_heap_end;
    return (void *) prev_heap_end;
}

int _isatty (int fd)
{
  return 1;
}

extern char system__text_io__initialized;
extern void system__text_io__initialize (void);
extern char system__text_io__is_tx_ready (void);
extern char system__text_io__is_rx_ready (void);
extern char system__text_io__use_cr_lf_for_new_line (void);
extern void system__text_io__put (char);
extern char system__text_io__get (void);

static void write_console (char c) {
  while (!system__text_io__is_tx_ready ())
    {
      continue;
    }

  system__text_io__put (c);
}

static char read_console (void)
{
  while (!system__text_io__is_rx_ready ())
    {
      continue;
    }
  return system__text_io__get ();
}

int _write (int fd, char *buf, int nbytes)
{
  int i;

  if (!system__text_io__initialized)
    {
      system__text_io__initialize ();
    }

  for (i = 0; i < nbytes; i++)
    {
      char c = buf[i];

      if (c == '\n' && system__text_io__use_cr_lf_for_new_line ()) {
          write_console ('\r');
        }
      write_console (c);
    }

  return nbytes;
}

int _close (int fd)
{
  return 0;
}

int _fstat (int fd, struct stat*buf)
{
  return -1;
}

off_t _lseek (int fd, off_t offset, int whence)
{
  errno = ESPIPE;
  return -1;
}

int _read (int fd, char *buf, int count)
{
  int i;

  if (!system__text_io__initialized)
    {
      system__text_io__initialize ();
    }

  for (i = 0; i < count;)
    {
      char c = read_console ();

      if (c == '\r' && system__text_io__use_cr_lf_for_new_line ())
        {
          continue;
        }
      buf[i++] = c;

      if (c == '\n')
        {
          break;
        }
    }
  return i;
}

int _kill (pid_t pid, int sig)
{
  return EINVAL;
}

pid_t _getpid (void)
{
  return 0;
}
