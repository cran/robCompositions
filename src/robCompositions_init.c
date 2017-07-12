#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C source code.
*/

/* .C calls */
extern void da(void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"da", (DL_FUNC) &da, 6},
  {NULL, NULL, 0}
};

void R_init_robCompositions(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
