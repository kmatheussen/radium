extern LANGSPEC wchar_t *talloc_wcsdup__(const wchar_t *input, const char *filename, int linenumber) __attribute__((malloc)); // __attribute__((returns_nonnull));

#define talloc_wcsdup(a) talloc_wcsdup__(a,__FILE__,__LINE__)

