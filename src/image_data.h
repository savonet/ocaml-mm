#ifdef HAVE_MEMALIGN
// Some systems have memalign() but no declaration for it.
void *memalign(size_t align, size_t size);
#else
// Assume malloc alignment is sufficient (the 0*align is to avoid an unused
// variable warning).
#define memalign(align,size) malloc(size+0*align)
#endif

// Default alignment
#define ALIGNMENT_BYTES 16
