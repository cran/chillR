#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _chillR_PhenoFlex(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

/* .Fortran calls */
extern void F77_NAME(tauk2)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
  {"_chillR_PhenoFlex", (DL_FUNC) &_chillR_PhenoFlex, 19},
  {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
  {"tauk2", (DL_FUNC) &F77_NAME(tauk2), 11},
  {NULL, NULL, 0}
};

void R_init_chillR(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
