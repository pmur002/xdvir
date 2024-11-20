
#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
#include "freetype.h"

static const R_CallMethodDef callMethods[] = {
    {"glyphMetrics", (DL_FUNC) &glyphMetrics, 1},
    { NULL, NULL, 0 }
};

void R_init_xdvir(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
