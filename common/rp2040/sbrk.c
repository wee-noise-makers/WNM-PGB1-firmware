#include <unistd.h>
#include <stdbool.h>

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

