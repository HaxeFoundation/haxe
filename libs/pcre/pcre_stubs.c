/*
   PCRE-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#if defined(_WIN32)
// #  define snprintf _snprintf
#  if defined(_DLL)
#    define PCREextern __declspec(dllexport)
#  else
#    define PCREextern
#  endif
#endif

#if _WIN64
  typedef long long *ovec_dst_ptr;
#else
  typedef long *ovec_dst_ptr;
#endif

#if __GNUC__ >= 3
# define inline inline __attribute__ ((always_inline))
# define __unused __attribute__ ((unused))
#else
# define __unused
# define inline
#endif

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include "pcre.h"

/* Error codes as defined for pcre 7.9, undefined in pcre 4.5 */
#ifndef PCRE_ERROR_PARTIAL
#define PCRE_ERROR_PARTIAL        (-12)
#endif
#ifndef PCRE_ERROR_BADPARTIAL
#define PCRE_ERROR_BADPARTIAL     (-13)
#endif
#ifndef PCRE_ERROR_RECURSIONLIMIT
#define PCRE_ERROR_RECURSIONLIMIT (-21)
#endif

typedef const unsigned char *chartables;  /* Type of chartable sets */

/* Contents of callout data */
struct cod {
  long subj_start;        /* Start of subject string */
  value *v_substrings_p;  /* Pointer to substrings matched so far */
  value *v_cof_p;         /* Pointer to callout function */
  value v_exn;            /* Possible exception raised by callout function */
};

/* Cache for exceptions */
static value *pcre_exc_Error         = NULL;  /* Exception [Error] */
static value *pcre_exc_Backtrack     = NULL;  /* Exception [Backtrack] */

/* Cache for polymorphic variants */
static value var_Start_only;   /* Variant [`Start_only] */
static value var_ANCHORED;     /* Variant [`ANCHORED] */
static value var_Char;         /* Variant [`Char char] */
static value var_Not_studied;  /* Variant [`Not_studied] */
static value var_Studied;      /* Variant [`Studied] */
static value var_Optimal;      /* Variant [`Optimal] */

static value None = Val_int(0);

/* Converts subject offsets from C-integers to OCaml-Integers.

   This is a bit tricky, because there are 32- and 64-bit platforms around
   and OCaml chooses the larger possibility for representing integers when
   available (also in arrays) - not so the PCRE!
*/
static inline void copy_ovector(
  long subj_start, const int *ovec_src, ovec_dst_ptr ovec_dst, int subgroups2)
{
  if (subj_start == 0)
    while (subgroups2--) {
      *ovec_dst = Val_int(*ovec_src);
      --ovec_src; --ovec_dst;
    }
  else
    while (subgroups2--) {
      *ovec_dst = Val_long(*ovec_src + subj_start);
      --ovec_src; --ovec_dst;
    }
}

/* Callout handler */
static int pcre_callout_handler(pcre_callout_block* cb)
{
  struct cod *cod = (struct cod *) cb->callout_data;

  if (cod != NULL) {
    /* Callout is available */
    value v_res;

    /* Set up parameter array */
    value v_callout_data = caml_alloc_small(8, 0);

    const value v_substrings = *cod->v_substrings_p;

    const int capture_top = cb->capture_top;
    int subgroups2 = capture_top << 1;
    const int subgroups2_1 = subgroups2 - 1;

    const int *ovec_src = cb->offset_vector + subgroups2_1;
    ovec_dst_ptr ovec_dst = &Field(Field(v_substrings, 1), 0) + subgroups2_1;
    long subj_start = cod->subj_start;

    copy_ovector(subj_start, ovec_src, ovec_dst, subgroups2);

    Field(v_callout_data, 0) = Val_int(cb->callout_number);
    Field(v_callout_data, 1) = v_substrings;
    Field(v_callout_data, 2) = Val_int(cb->start_match + subj_start);
    Field(v_callout_data, 3) = Val_int(cb->current_position + subj_start);
    Field(v_callout_data, 4) = Val_int(capture_top);
    Field(v_callout_data, 5) = Val_int(cb->capture_last);
    Field(v_callout_data, 6) = Val_int(cb->pattern_position);
    Field(v_callout_data, 7) = Val_int(cb->next_item_length);

    /* Perform callout */
    v_res = caml_callback_exn(*cod->v_cof_p, v_callout_data);

    if (Is_exception_result(v_res)) {
      /* Callout raised an exception */
      const value v_exn = Extract_exception(v_res);
      if (Field(v_exn, 0) == *pcre_exc_Backtrack) return 1;
      cod->v_exn = v_exn;
      return PCRE_ERROR_CALLOUT;
    }
  }

  return 0;
}

/* Fetchs the named OCaml-values + caches them and
   calculates + caches the variant hash values */
CAMLprim value pcre_ocaml_init(value __unused v_unit)
{
  pcre_exc_Error     = caml_named_value("Pcre.Error");
  pcre_exc_Backtrack = caml_named_value("Pcre.Backtrack");

  var_Start_only  = caml_hash_variant("Start_only");
  var_ANCHORED    = caml_hash_variant("ANCHORED");
  var_Char        = caml_hash_variant("Char");
  var_Not_studied = caml_hash_variant("Not_studied");
  var_Studied     = caml_hash_variant("Studied");
  var_Optimal     = caml_hash_variant("Optimal");

  pcre_callout = &pcre_callout_handler;

  return Val_unit;
}

/* Finalizing deallocation function for chartable sets */
static void pcre_dealloc_tables(value v_table)
{ (pcre_free)((void *) Field(v_table, 1)); }

/* Finalizing deallocation function for compiled regular expressions */
static void pcre_dealloc_regexp(value v_rex)
{
  void *extra = (void *) Field(v_rex, 2);
  (pcre_free)((void *) Field(v_rex, 1));
  if (extra != NULL)
#ifdef PCRE_STUDY_JIT_COMPILE
    pcre_free_study(extra);
#else
    pcre_free(extra);
#endif
}

/* Makes OCaml-string from PCRE-version */
CAMLprim value pcre_version_stub(value __unused v_unit)
{
  return caml_copy_string((char *) pcre_version());
}


/* Raising exceptions */

static inline void raise_pcre_error(value v_arg) Noreturn;
static inline void raise_partial() Noreturn;
static inline void raise_bad_partial() Noreturn;
static inline void raise_bad_utf8() Noreturn;
static inline void raise_bad_utf8_offset() Noreturn;
static inline void raise_match_limit() Noreturn;
static inline void raise_recursion_limit() Noreturn;
static inline void raise_bad_pattern(const char *msg, int pos) Noreturn;
static inline void raise_internal_error(char *msg) Noreturn;

static inline void raise_pcre_error(value v_arg)
{ caml_raise_with_arg(*pcre_exc_Error, v_arg); }

static inline void raise_partial() { raise_pcre_error(Val_int(0)); }
static inline void raise_bad_partial() { raise_pcre_error(Val_int(1)); }
static inline void raise_bad_utf8() { raise_pcre_error(Val_int(2)); }
static inline void raise_bad_utf8_offset() { raise_pcre_error(Val_int(3)); }
static inline void raise_match_limit() { raise_pcre_error(Val_int(4)); }
static inline void raise_recursion_limit() { raise_pcre_error(Val_int(5)); }

static inline void raise_bad_pattern(const char *msg, int pos)
{
  CAMLparam0();
  CAMLlocal1(v_msg);
  value v_arg;
  v_msg = caml_copy_string(msg);
  v_arg = caml_alloc_small(2, 0);
  Field(v_arg, 0) = v_msg;
  Field(v_arg, 1) = Val_int(pos);
  raise_pcre_error(v_arg);
  CAMLnoreturn;
}

static inline void raise_internal_error(char *msg)
{
  CAMLparam0();
  CAMLlocal1(v_msg);
  value v_arg;
  v_msg = caml_copy_string(msg);
  v_arg = caml_alloc_small(1, 1);
  Field(v_arg, 0) = v_msg;
  raise_pcre_error(v_arg);
  CAMLnoreturn;
}

/* PCRE pattern compilation */

/* Makes compiled regular expression from compilation options, an optional
   value of chartables and the pattern string */
CAMLprim value pcre_compile_stub(value v_opt, value v_tables, value v_pat)
{
  value v_rex;  /* Final result -> value of type [regexp] */
  const char *error = NULL;  /* pointer to possible error message */
  int error_ofs = 0;  /* offset in the pattern at which error occurred */

  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  chartables tables =
    (v_tables == None) ? NULL : (chartables) Field(Field(v_tables, 0), 1);

  /* Compiles the pattern */
  pcre *regexp = pcre_compile(String_val(v_pat), Int_val(v_opt), &error,
                              &error_ofs, tables);

  /* Raises appropriate exception with [BadPattern] if the pattern
     could not be compiled */
  if (regexp == NULL) raise_bad_pattern(error, error_ofs);

  /* GC will do a full cycle every 1_000_000 regexp allocations (a typical
     regexp probably consumes less than 100 bytes -> maximum of 100_000_000
     bytes unreclaimed regexps) */
  v_rex = caml_alloc_final(4, pcre_dealloc_regexp, 1, 1000000);

  /* Field[1]: compiled regular expression (Field[0] is finalizing
     function! See above!) */
  Field(v_rex, 1) = (value) regexp;

  /* Field[2]: extra information about regexp when it has been studied
     successfully */
  Field(v_rex, 2) = (value) NULL;

  /* Field[3]: If 0 -> regexp has not yet been studied
                  1 -> regexp has already been studied */
  Field(v_rex, 3) = 0;

  return v_rex;
}

/* Studies a regexp */
CAMLprim value pcre_study_stub(value v_rex)
{
  /* If it has not yet been studied */
  if (! (int) Field(v_rex, 3)) {
    const char *error = NULL;
    pcre_extra *extra = pcre_study((pcre *) Field(v_rex, 1), 0, &error);
    if (error != NULL) caml_invalid_argument((char *) error);
    Field(v_rex, 2) = (value) extra;
    Field(v_rex, 3) = Val_int(1);
  }
  return v_rex;
}

/* Sets a match limit recursion for a regular expression imperatively */
CAMLprim value pcre_set_imp_match_limit_recursion_stub(value v_rex, value v_lim)
{
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) {
    extra = pcre_malloc(sizeof(pcre_extra));
    extra->flags = PCRE_EXTRA_MATCH_LIMIT_RECURSION;
    Field(v_rex, 2) = (value) extra;
  } else {
    unsigned long *flags_ptr = &extra->flags;
    *flags_ptr = PCRE_EXTRA_MATCH_LIMIT_RECURSION | *flags_ptr;
  }
  extra->match_limit_recursion = Int_val(v_lim);
  return v_rex;
}

/* Gets the match limit recursion of a regular expression if it exists */
CAMLprim value pcre_get_match_limit_recursion_stub(value v_rex)
{
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) return None;
  if (extra->flags & PCRE_EXTRA_MATCH_LIMIT_RECURSION) {
    value v_lim = Val_int(extra->match_limit_recursion);
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_lim;
    return v_res;
  }
  return None;
}

/* Sets a match limit for a regular expression imperatively */
CAMLprim value pcre_set_imp_match_limit_stub(value v_rex, value v_lim)
{
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) {
    extra = pcre_malloc(sizeof(pcre_extra));
    extra->flags = PCRE_EXTRA_MATCH_LIMIT;
    Field(v_rex, 2) = (value) extra;
  } else {
    unsigned long *flags_ptr = &extra->flags;
    *flags_ptr = PCRE_EXTRA_MATCH_LIMIT | *flags_ptr;
  }
  extra->match_limit = Int_val(v_lim);
  return v_rex;
}

/* Gets the match limit of a regular expression if it exists */
CAMLprim value pcre_get_match_limit_stub(value v_rex)
{
  pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);
  if (extra == NULL) return None;
  if (extra->flags & PCRE_EXTRA_MATCH_LIMIT) {
    value v_lim = Val_int(extra->match_limit);
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = v_lim;
    return v_res;
  }
  return None;
}

/* Performs the call to the pcre_fullinfo function */
static inline int pcre_fullinfo_stub(value v_rex, int what, void *where)
{
  return pcre_fullinfo((pcre *) Field(v_rex, 1), (pcre_extra *) Field(v_rex, 2),
                       what, where);
}

/* Some stubs for info-functions */

/* Generic macro for getting integer results from pcre_fullinfo */
#define make_info(tp, cnv, name, option) \
  CAMLprim value pcre_##name##_stub(value v_rex) \
  { \
    tp options; \
    const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_##option, &options); \
    if (ret != 0) raise_internal_error("pcre_##name##_stub"); \
    return cnv(options); \
  }

make_info(unsigned long, Val_long, options, OPTIONS)
make_info(size_t, Val_long, size, SIZE)
make_info(size_t, Val_long, studysize, STUDYSIZE)
make_info(int, Val_int, capturecount, CAPTURECOUNT)
make_info(int, Val_int, backrefmax, BACKREFMAX)
make_info(int, Val_int, namecount, NAMECOUNT)
make_info(int, Val_int, nameentrysize, NAMEENTRYSIZE)

CAMLprim value pcre_firstbyte_stub(value v_rex)
{
  int firstbyte;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTBYTE, &firstbyte);

  if (ret != 0) raise_internal_error("pcre_firstbyte_stub");

  switch (firstbyte) {
    case -1 : return var_Start_only; break;  /* [`Start_only] */
    case -2 : return var_ANCHORED; break;    /* [`ANCHORED] */
    default :
      if (firstbyte < 0 )  /* Should not happen */
        raise_internal_error("pcre_firstbyte_stub");
      else {
        value v_firstbyte;
        /* Allocates the non-constant constructor [`Char of char] and fills
           in the appropriate value */
        v_firstbyte = caml_alloc_small(2, 0);
        Field(v_firstbyte, 0) = var_Char;
        Field(v_firstbyte, 1) = Val_int(firstbyte);
        return v_firstbyte;
      }
  }
}

CAMLprim value pcre_firsttable_stub(value v_rex)
{
  const unsigned char *ftable;

  int ret =
    pcre_fullinfo_stub(v_rex, PCRE_INFO_FIRSTTABLE, (void *) &ftable);

  if (ret != 0) raise_internal_error("pcre_firsttable_stub");

  if (ftable == NULL) return None;
  else {
    value v_res, v_res_str;
    char *ptr;
    int i;

    Begin_roots1(v_rex);
      v_res_str = caml_alloc_string(32);
    End_roots();

    ptr = String_val(v_res_str);
    for (i = 0; i <= 31; ++i) { *ptr = *ftable; ++ptr; ++ftable; }

    Begin_roots1(v_res_str);
      /* Allocates [Some string] from firsttable */
      v_res = caml_alloc_small(1, 0);
    End_roots();

    Field(v_res, 0) = v_res_str;

    return v_res;
  }
}

CAMLprim value pcre_lastliteral_stub(value v_rex)
{
  int lastliteral;
  const int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_LASTLITERAL,
                                        &lastliteral);

  if (ret != 0) raise_internal_error("pcre_lastliteral_stub");

  if (lastliteral == -1) return None;
  if (lastliteral < 0) raise_internal_error("pcre_lastliteral_stub");
  else {
    /* Allocates [Some char] */
    value v_res = caml_alloc_small(1, 0);
    Field(v_res, 0) = Val_int(lastliteral);
    return v_res;
  }
}

CAMLprim value pcre_study_stat_stub(value v_rex)
{
  /* Generates the appropriate constant constructor [`Optimal] or
     [`Studied] if regexp has already been studied */
  if (Field(v_rex, 3))
    return ((pcre_extra *) Field(v_rex, 2) == NULL) ? var_Optimal : var_Studied;

  return var_Not_studied;  /* otherwise [`Not_studied] */
}

static inline void handle_exec_error(char *loc, const int ret) Noreturn;

static inline void handle_exec_error(char *loc, const int ret)
{
  switch (ret) {
    /* Dedicated exceptions */
    case PCRE_ERROR_NOMATCH : caml_raise_not_found();
    case PCRE_ERROR_PARTIAL : raise_partial();
    case PCRE_ERROR_MATCHLIMIT : raise_match_limit();
    case PCRE_ERROR_BADPARTIAL : raise_bad_partial();
    case PCRE_ERROR_BADUTF8 : raise_bad_utf8();
    case PCRE_ERROR_BADUTF8_OFFSET : raise_bad_utf8_offset();
    case PCRE_ERROR_RECURSIONLIMIT : raise_recursion_limit();
    /* Unknown error */
    default : {
      char err_buf[100];
      snprintf(err_buf, 100, "%s: unhandled PCRE error code: %d", loc, ret);
      raise_internal_error(err_buf);
    }
  }
}

static inline void handle_pcre_exec_result(
  int *ovec, value v_ovec, long ovec_len, long subj_start, int ret)
{
  ovec_dst_ptr ocaml_ovec = (ovec_dst_ptr) &Field(v_ovec, 0);
  const int subgroups2 = ret * 2;
  const int subgroups2_1 = subgroups2 - 1;
  const int *ovec_src = ovec + subgroups2_1;
  ovec_dst_ptr ovec_clear_stop = ocaml_ovec + (ovec_len * 2) / 3;
  ovec_dst_ptr ovec_dst = ocaml_ovec + subgroups2_1;
  copy_ovector(subj_start, ovec_src, ovec_dst, subgroups2);
  while (++ovec_dst < ovec_clear_stop) *ovec_dst = -1;
}

/* Executes a pattern match with runtime options, a regular expression, a
   matching position, the start of the the subject string, a subject string,
   a number of subgroup offsets, an offset vector and an optional callout
   function */
CAMLprim value pcre_exec_stub(value v_opt, value v_rex, value v_pos,
                              value v_subj_start, value v_subj,
                              value v_ovec, value v_maybe_cof)
{
  int ret;
  long
    pos = Long_val(v_pos),
    len = caml_string_length(v_subj),
    subj_start = Long_val(v_subj_start);
  long ovec_len = Wosize_val(v_ovec);

  if (pos > len || pos < subj_start)
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal position");

  if (subj_start > len || subj_start < 0)
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal subject start");

  pos -= subj_start;
  len -= subj_start;

  {
    const pcre *code = (pcre *) Field(v_rex, 1);  /* Compiled pattern */
    const pcre_extra *extra = (pcre_extra *) Field(v_rex, 2);  /* Extra info */
    const char *ocaml_subj =
      String_val(v_subj) + subj_start;  /* Subject string */
    const int opt = Int_val(v_opt);  /* Runtime options */

    /* Special case when no callout functions specified */
    if (v_maybe_cof == None) {
      int *ovec = (int *) &Field(v_ovec, 0);

      /* Performs the match */
      ret = pcre_exec(code, extra, ocaml_subj, len, pos, opt, ovec, ovec_len);

      if (ret < 0) handle_exec_error("pcre_exec_stub", ret);
      else handle_pcre_exec_result(ovec, v_ovec, ovec_len, subj_start, ret);
    }

    /* There are callout functions */
    else {
      value v_cof = Field(v_maybe_cof, 0);
      value v_substrings;
      char *subj = caml_stat_alloc(sizeof(char) * len);
      int *ovec = caml_stat_alloc(sizeof(int) * ovec_len);
      struct cod cod = { 0, (value *) NULL, (value *) NULL, (value) NULL };
      struct pcre_extra new_extra =
#ifdef PCRE_EXTRA_MATCH_LIMIT_RECURSION
# ifdef PCRE_EXTRA_MARK
#  ifdef PCRE_EXTRA_EXECUTABLE_JIT
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0, NULL, NULL };
#  else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0, NULL };
#  endif
# else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL, 0 };
# endif
#else
        { PCRE_EXTRA_CALLOUT_DATA, NULL, 0, NULL, NULL };
#endif

      cod.subj_start = subj_start;
      memcpy(subj, ocaml_subj, len);

      Begin_roots4(v_rex, v_cof, v_substrings, v_ovec);
        Begin_roots1(v_subj);
          v_substrings = caml_alloc_small(2, 0);
        End_roots();

        Field(v_substrings, 0) = v_subj;
        Field(v_substrings, 1) = v_ovec;

        cod.v_substrings_p = &v_substrings;
        cod.v_cof_p = &v_cof;
        new_extra.callout_data = &cod;

        if (extra == NULL) {
          ret = pcre_exec(code, &new_extra, subj, len, pos, opt, ovec,
                          ovec_len);
        }
        else {
          new_extra.flags = PCRE_EXTRA_CALLOUT_DATA | extra->flags;
          new_extra.study_data = extra->study_data;
          new_extra.match_limit = extra->match_limit;
          new_extra.tables = extra->tables;
#ifdef PCRE_EXTRA_MATCH_LIMIT_RECURSION
          new_extra.match_limit_recursion = extra->match_limit_recursion;
#endif

          ret = pcre_exec(code, &new_extra, subj, len, pos, opt, ovec,
                          ovec_len);
        }

        caml_stat_free(subj);
      End_roots();

      if (ret < 0) {
        caml_stat_free(ovec);
        if (ret == PCRE_ERROR_CALLOUT) caml_raise(cod.v_exn);
        else handle_exec_error("pcre_exec_stub(callout)", ret);
      } else {
        handle_pcre_exec_result(ovec, v_ovec, ovec_len, subj_start, ret);
        caml_stat_free(ovec);
      }
    }
  }

  return Val_unit;
}

/* Byte-code hook for pcre_exec_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre_exec_stub_bc(value *argv, int __unused argn)
{
  return pcre_exec_stub(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

/* Generates a new set of chartables for the current locale (see man
   page of PCRE */
CAMLprim value pcre_maketables_stub(value __unused v_unit)
{
  /* GC will do a full cycle every 1_000_000 table set allocations (one
     table set consumes 864 bytes -> maximum of 864_000_000 bytes unreclaimed
     table sets) */
  const value v_res = caml_alloc_final(2, pcre_dealloc_tables, 1, 1000000);
  Field(v_res, 1) = (value) pcre_maketables();
  return v_res;
}

/* Wraps around the isspace-function */
CAMLprim value pcre_isspace_stub(value v_c)
{
  return Val_bool(isspace(Int_val(v_c)));
}

/* Returns number of substring associated with a name */
CAMLprim value pcre_get_stringnumber_stub(value v_rex, value v_name)
{
  const int ret = pcre_get_stringnumber((pcre *) Field(v_rex, 1),
                                        String_val(v_name));
  if (ret == PCRE_ERROR_NOSUBSTRING)
    caml_invalid_argument("Named string not found");

  return Val_int(ret);
}

/* Returns array of names of named substrings in a regexp */
CAMLprim value pcre_names_stub(value v_rex)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  int name_count;
  int entry_size;
  const char *tbl_ptr;
  int i;

  int ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMECOUNT, &name_count);
  if (ret != 0) raise_internal_error("pcre_names_stub: namecount");

  ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMEENTRYSIZE, &entry_size);
  if (ret != 0) raise_internal_error("pcre_names_stub: nameentrysize");

  ret = pcre_fullinfo_stub(v_rex, PCRE_INFO_NAMETABLE, &tbl_ptr);
  if (ret != 0) raise_internal_error("pcre_names_stub: nametable");

  v_res = caml_alloc(name_count, 0);

  for (i = 0; i < name_count; ++i) {
    value v_name = caml_copy_string(tbl_ptr + 2);
    Store_field(v_res, i, v_name);
    tbl_ptr += entry_size;
  }

  CAMLreturn(v_res);
}

/* Generic stub for getting integer results from pcre_config */
static inline int pcre_config_int(int what)
{
  int ret;
  pcre_config(what, (void *) &ret);
  return ret;
}

/* Generic stub for getting long integer results from pcre_config */
static inline int pcre_config_long(int what)
{
  long ret;
  pcre_config(what, (void *) &ret);
  return ret;
}

/* Some stubs for config-functions */

/* Returns boolean indicating UTF8-support */
CAMLprim value pcre_config_utf8_stub(value __unused v_unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_UTF8)); }

/* Returns character used as newline */
CAMLprim value pcre_config_newline_stub(value __unused v_unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_NEWLINE)); }

/* Returns number of bytes used for internal linkage of regular expressions */
CAMLprim value pcre_config_link_size_stub(value __unused v_unit)
{ return Val_int(pcre_config_int(PCRE_CONFIG_LINK_SIZE)); }

/* Returns boolean indicating use of stack recursion */
CAMLprim value pcre_config_stackrecurse_stub(value __unused v_unit)
{ return Val_bool(pcre_config_int(PCRE_CONFIG_STACKRECURSE)); }

/* Returns default limit for calls to internal matching function */
CAMLprim value pcre_config_match_limit_stub(value __unused v_unit)
{ return Val_long(pcre_config_long(PCRE_CONFIG_MATCH_LIMIT)); }

/* Returns default limit for calls to internal matching function */
CAMLprim value pcre_config_match_limit_recursion_stub(value __unused v_unit)
{ return Val_long(pcre_config_long(PCRE_CONFIG_MATCH_LIMIT_RECURSION)); }
