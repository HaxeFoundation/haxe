/*
   PCRE2-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#if defined(_WIN32)
#  define snprintf _snprintf
#  if defined(_DLL)
#    define PCREextern __declspec(dllexport)
#  else
#    define PCREextern
#  endif
#endif

#if _WIN64
  typedef long long *caml_int_ptr;
#else
  typedef long *caml_int_ptr;
#endif

#if __GNUC__ >= 3
# define __unused __attribute__ ((unused))
#else
# define __unused
#endif

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>

#define PCRE2_CODE_UNIT_WIDTH 8

#include <pcre2.h>

typedef const unsigned char *chartables;  /* Type of chartable sets */

/* Contents of callout data */
struct cod {
  long subj_start;        /* Start of subject string */
  value *v_substrings_p;  /* Pointer to substrings matched so far */
  value *v_cof_p;         /* Pointer to callout function */
  value v_exn;            /* Possible exception raised by callout function */
};

/* Cache for exceptions */
static const value *pcre2_exc_Error     = NULL;  /* Exception [Error] */
static const value *pcre2_exc_Backtrack = NULL;  /* Exception [Backtrack] */

/* Cache for polymorphic variants */
static value var_Start_only;   /* Variant [`Start_only] */
static value var_ANCHORED;     /* Variant [`ANCHORED] */
static value var_Char;         /* Variant [`Char char] */

static value None = Val_int(0);

/* Data associated with OCaml values of PCRE regular expression */
struct pcre2_ocaml_regexp { pcre2_code *rex; pcre2_match_context *mcontext; };

#define Pcre2_ocaml_regexp_val(v) \
  ((struct pcre2_ocaml_regexp *) Data_custom_val(v))

#define get_rex(v) Pcre2_ocaml_regexp_val(v)->rex
#define get_mcontext(v) Pcre2_ocaml_regexp_val(v)->mcontext

#define set_rex(v, r) Pcre2_ocaml_regexp_val(v)->rex = r
#define set_mcontext(v, c) Pcre2_ocaml_regexp_val(v)->mcontext = c

/* Data associated with OCaml values of PCRE tables */
struct pcre2_ocaml_tables { chartables tables; };

#define Pcre2_ocaml_tables_val(v) \
  ((struct pcre2_ocaml_tables *) Data_custom_val(v))

#define get_tables(v) Pcre2_ocaml_tables_val(v)->tables
#define set_tables(v, t) Pcre2_ocaml_tables_val(v)->tables = t

/* Converts subject offsets from C-integers to OCaml-Integers.

   This is a bit tricky, because there are 32- and 64-bit platforms around
   and OCaml chooses the larger possibility for representing integers when
   available (also in arrays) - not so the PCRE!
*/
static inline void copy_ovector(
  long subj_start, const size_t* ovec_src, caml_int_ptr ovec_dst, uint32_t subgroups2)
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
static int pcre2_callout_handler(pcre2_callout_block* cb, struct cod* cod)
{
  if (cod != NULL) {
    /* Callout is available */
    value v_res;

    /* Set up parameter array */
    value v_callout_data = caml_alloc_small(8, 0);

    const value v_substrings = *cod->v_substrings_p;

    const uint32_t capture_top = cb->capture_top;
    uint32_t subgroups2 = capture_top << 1;
    const uint32_t subgroups2_1 = subgroups2 - 1;

    const size_t *ovec_src = cb->offset_vector + subgroups2_1;
    caml_int_ptr ovec_dst = &Field(Field(v_substrings, 1), 0) + subgroups2_1;
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
      if (Field(v_exn, 0) == *pcre2_exc_Backtrack) return 1;
      cod->v_exn = v_exn;
      return PCRE2_ERROR_CALLOUT;
    }
  }

  return 0;
}

/* Fetches the named OCaml-values + caches them and
   calculates + caches the variant hash values */
CAMLprim value pcre2_ocaml_init(value __unused v_unit)
{
  pcre2_exc_Error     = caml_named_value("Pcre2.Error");
  pcre2_exc_Backtrack = caml_named_value("Pcre2.Backtrack");

  var_Start_only  = caml_hash_variant("Start_only");
  var_ANCHORED    = caml_hash_variant("ANCHORED");
  var_Char        = caml_hash_variant("Char");

  return Val_unit;
}

/* Finalizing deallocation function for chartable sets */
static void pcre2_dealloc_tables(value v_tables)
{
#if PCRE2_MINOR >= 34
  pcre2_maketables_free(NULL, get_tables(v_tables));
#else
  free((void*)get_tables(v_tables));
#endif
}

/* Finalizing deallocation function for compiled regular expressions */
static void pcre2_dealloc_regexp(value v_rex)
{
  pcre2_code_free(get_rex(v_rex));
  pcre2_match_context_free(get_mcontext(v_rex));
}


/* Raising exceptions */

CAMLnoreturn_start
static inline void raise_pcre2_error(value v_arg)
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_partial()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_utf()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_utf_offset()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_match_limit()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_depth_limit()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_workspace_size()
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_bad_pattern(int code, size_t pos)
CAMLnoreturn_end;

CAMLnoreturn_start
static inline void raise_internal_error(char *msg)
CAMLnoreturn_end;

static inline void raise_pcre2_error(value v_arg)
{ caml_raise_with_arg(*pcre2_exc_Error, v_arg); }

static inline void raise_partial() { raise_pcre2_error(Val_int(0)); }
static inline void raise_bad_utf() { raise_pcre2_error(Val_int(1)); }
static inline void raise_bad_utf_offset() { raise_pcre2_error(Val_int(2)); }
static inline void raise_match_limit() { raise_pcre2_error(Val_int(3)); }
static inline void raise_depth_limit() { raise_pcre2_error(Val_int(4)); }
static inline void raise_workspace_size() { raise_pcre2_error(Val_int(5)); }

static inline void raise_bad_pattern(int code, size_t pos)
{
  CAMLparam0();
  CAMLlocal1(v_msg);
  value v_arg;
  v_msg = caml_alloc_string(128);
  pcre2_get_error_message(code, (PCRE2_UCHAR *)String_val(v_msg), 128);
  v_arg = caml_alloc_small(2, 0);
  Field(v_arg, 0) = v_msg;
  Field(v_arg, 1) = Val_int(pos);
  raise_pcre2_error(v_arg);
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
  raise_pcre2_error(v_arg);
  CAMLnoreturn;
}

/* PCRE pattern compilation */

static struct custom_operations regexp_ops = {
  "pcre2_ocaml_regexp",
  pcre2_dealloc_regexp,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

/* Makes compiled regular expression from compilation options, an optional
   value of chartables and the pattern string */

CAMLprim value pcre2_compile_stub(int64_t v_opt, value v_tables, value v_pat)
{
  value v_rex;  /* Final result -> value of type [regexp] */
  int error_code = 0;  /* error code for potential error */
  size_t error_ofs = 0;  /* offset in the pattern at which error occurred */
  size_t length = caml_string_length(v_pat);

  pcre2_compile_context* ccontext = NULL;
  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  if (v_tables != None) {
    ccontext = pcre2_compile_context_create(NULL);
    pcre2_set_character_tables(ccontext, get_tables(Field(v_tables, 0)));
  }

  /* Compiles the pattern */
  pcre2_code* regexp = pcre2_compile((PCRE2_SPTR)String_val(v_pat), length, v_opt,
                                     &error_code, &error_ofs, ccontext);

  pcre2_compile_context_free(ccontext);

  /* Raises appropriate exception with [BadPattern] if the pattern
     could not be compiled */
  if (regexp == NULL) raise_bad_pattern(error_code, error_ofs);

  /* GC will do a full cycle every 1_000_000 regexp allocations (a typical
     regexp probably consumes less than 100 bytes -> maximum of 100_000_000
     bytes unreclaimed regexps) */
  v_rex =
    caml_alloc_custom(&regexp_ops,
      sizeof(struct pcre2_ocaml_regexp), 1, 1000000);

  set_rex(v_rex, regexp);
  set_mcontext(v_rex, pcre2_match_context_create(NULL));

  return v_rex;
}

CAMLprim value pcre2_compile_stub_bc(value v_opt, value v_tables, value v_pat)
{
  return pcre2_compile_stub(Long_val(v_opt), v_tables, v_pat);
}

/* Gets the depth limit of a regular expression if it exists */
/* CAMLprim value pcre2_get_depth_limit_stub(value v_rex); */

/* Gets the match limit of a regular expression if it exists */
/* CAMLprim value pcre2_get_match_limit_stub(value v_rex); */


/* Sets a match limit for a regular expression imperatively */

CAMLprim value pcre2_set_imp_match_limit_stub(value v_rex, value v_lim) {
  pcre2_match_context* mcontext = get_mcontext(v_rex);
  pcre2_set_match_limit(mcontext, v_lim);
  return v_rex;
}

CAMLprim value pcre2_set_imp_match_limit_stub_bc(value v_rex, value v_lim)
{
  return pcre2_set_imp_match_limit_stub(v_rex, Int_val(v_lim));
}


/* Sets a depth limit for a regular expression imperatively */

CAMLprim value pcre2_set_imp_depth_limit_stub(value v_rex, intnat v_lim) {
  pcre2_match_context* mcontext = get_mcontext(v_rex);
  pcre2_set_depth_limit(mcontext, v_lim);
  return v_rex;
}

CAMLprim value pcre2_set_imp_depth_limit_stub_bc(value v_rex, value v_lim)
{
  return pcre2_set_imp_depth_limit_stub(v_rex, Int_val(v_lim));
}


/* Performs the call to the pcre2_pattern_info function */
static inline int pcre2_pattern_info_stub(value v_rex, int what, void* where)
{
  return pcre2_pattern_info(get_rex(v_rex), what, where);
}

/* Some stubs for info-functions */

/* Generic macro for getting integer results from pcre2_pattern_info */
#define make_intnat_info(tp, name, option) \
  CAMLprim intnat pcre2_##name##_stub(value v_rex) \
  { \
    tp options; \
    const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_##option, &options); \
    if (ret != 0) raise_internal_error("pcre2_##name##_stub"); \
    return options; \
  } \
  \
  CAMLprim value pcre2_##name##_stub_bc(value v_rex) \
  { return Val_int(pcre2_##name##_stub(v_rex)); }

make_intnat_info(size_t, size, SIZE)
make_intnat_info(int, capturecount, CAPTURECOUNT)
make_intnat_info(int, backrefmax, BACKREFMAX)
make_intnat_info(int, namecount, NAMECOUNT)
make_intnat_info(int, nameentrysize, NAMEENTRYSIZE)

CAMLprim int64_t pcre2_argoptions_stub(value v_rex)
{
  uint32_t options;
  const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_ARGOPTIONS, &options);
  if (ret != 0) raise_internal_error("pcre2_##name##_stub");
  return (int64_t)options;
}

CAMLprim value pcre2_argoptions_stub_bc(value v_rex)
{ return Val_long(pcre2_argoptions_stub(v_rex)); }

CAMLprim value pcre2_firstcodeunit_stub(value v_rex)
{
  uint32_t firstcodetype;
  const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODETYPE, &firstcodetype);

  if (ret != 0) raise_internal_error("pcre2_firstcodeunit_stub");

  switch (firstcodetype) {
    case 2 : return var_Start_only; break;  /* [`Start_only] */
    case 0 : return var_ANCHORED; break;    /* [`ANCHORED] */
    case 1: {
      uint32_t firstcodeunit;
      const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODEUNIT, &firstcodeunit);
      if (ret != 0) raise_internal_error("pcre2_firstcodeunit_stub");

      value v_firstbyte;
      /* Allocates the non-constant constructor [`Char of char] and fills
         in the appropriate value */
      v_firstbyte = caml_alloc_small(2, 0);
      Field(v_firstbyte, 0) = var_Char;
      Field(v_firstbyte, 1) = Val_int(firstcodeunit);

      return v_firstbyte;
      break;
    }
    default: /* Should not happen */
      raise_internal_error("pcre2_firstcodeunit_stub");
  }
}

CAMLprim value pcre2_lastcodeunit_stub(value v_rex)
{
  uint32_t lastcodetype;
  const int ret =
    pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODETYPE, &lastcodetype);

  if (ret != 0) raise_internal_error("pcre2_lastcodeunit_stub");

  if (lastcodetype == 0) return None;
  if (lastcodetype != 1) raise_internal_error("pcre2_lastcodeunit_stub");
  else {
    /* Allocates [Some char] */
    value v_res = caml_alloc_small(1, 0);
    uint32_t lastcodeunit;
    const int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODEUNIT, &lastcodeunit);
    if (ret != 0) raise_internal_error("pcre2_lastcodeunit_stub");
    Field(v_res, 0) = Val_int(lastcodeunit);
    return v_res;
  }
}

CAMLnoreturn_start
static inline void handle_match_error(char *loc, const int ret)
CAMLnoreturn_end;

static inline void handle_match_error(char *loc, const int ret)
{
  switch (ret) {
    /* Dedicated exceptions */
    case PCRE2_ERROR_NOMATCH : caml_raise_not_found();
    case PCRE2_ERROR_PARTIAL : raise_partial();
    case PCRE2_ERROR_MATCHLIMIT : raise_match_limit();
    case PCRE2_ERROR_BADUTFOFFSET : raise_bad_utf_offset();
    case PCRE2_ERROR_DEPTHLIMIT : raise_depth_limit();
    case PCRE2_ERROR_DFA_WSSIZE : raise_workspace_size();
    default : {
      if (PCRE2_ERROR_UTF8_ERR21 <= ret && ret <= PCRE2_ERROR_UTF8_ERR1)
        raise_bad_utf();
      /* Unknown error */
      char err_buf[100];
      snprintf(err_buf, 100, "%s: unhandled PCRE2 error code: %d", loc, ret);
      raise_internal_error(err_buf);
    }
  }
}

static inline void handle_pcre2_match_result(
  size_t *ovec, value v_ovec, size_t ovec_len, long subj_start, uint32_t ret)
{
  caml_int_ptr ocaml_ovec = (caml_int_ptr) &Field(v_ovec, 0);
  const uint32_t subgroups2 = ret * 2;
  const uint32_t subgroups2_1 = subgroups2 - 1;
  const size_t *ovec_src = ovec + subgroups2_1;
  caml_int_ptr ovec_clear_stop = ocaml_ovec + (ovec_len * 2) / 3;
  caml_int_ptr ovec_dst = ocaml_ovec + subgroups2_1;
  copy_ovector(subj_start, ovec_src, ovec_dst, subgroups2);
  while (++ovec_dst < ovec_clear_stop) *ovec_dst = -1;
}

/* Executes a pattern match with runtime options, a regular expression, a
   matching position, the start of the the subject string, a subject string,
   a number of subgroup offsets, an offset vector and an optional callout
   function */

CAMLprim value pcre2_match_stub0(
    int64_t v_opt, value v_rex, intnat v_pos, intnat v_subj_start, value v_subj,
    value v_ovec, value v_maybe_cof, value v_workspace)
{
  int ret;
  int is_dfa = v_workspace != (value) NULL;
  long
    pos = v_pos,
    subj_start = v_subj_start;
  size_t
    ovec_len = Wosize_val(v_ovec),
    len = caml_string_length(v_subj);

  if (pos > (long)len || pos < subj_start)
    caml_invalid_argument("Pcre2.pcre2_match_stub: illegal position");

  if (subj_start > (long)len || subj_start < 0)
    caml_invalid_argument("Pcre2.pcre2_match_stub: illegal subject start");

  pos -= subj_start;
  len -= subj_start;

  {
    const pcre2_code *code = get_rex(v_rex);  /* Compiled pattern */
    pcre2_match_context* mcontext = get_mcontext(v_rex);  /* Match context */
    PCRE2_SPTR ocaml_subj = (PCRE2_SPTR)String_val(v_subj) + subj_start;  /* Subject string */

    pcre2_match_data* match_data = pcre2_match_data_create_from_pattern(code, NULL);

    /* Special case when no callout functions specified */
    if (v_maybe_cof == None) {
      /* Performs the match */
      if (is_dfa)
        ret =
          pcre2_dfa_match(code, ocaml_subj, len, pos, v_opt, match_data, mcontext,
              (int *) &Field(v_workspace, 0), Wosize_val(v_workspace));
      else
        ret = pcre2_match(code, ocaml_subj, len, pos, v_opt, match_data, mcontext);

      size_t *ovec = pcre2_get_ovector_pointer(match_data);

      if (ret < 0) {
        pcre2_match_data_free(match_data);
        handle_match_error("pcre2_match_stub", ret);
      } else {
        handle_pcre2_match_result(ovec, v_ovec, ovec_len, subj_start, ret);
      }
    }

    /* There are callout functions */
    else {
      value v_cof = Field(v_maybe_cof, 0);
      value v_substrings;
      PCRE2_UCHAR* subj = caml_stat_alloc(sizeof(char) * len);
      int workspace_len;
      int *workspace;
      struct cod cod = { 0, (value *) NULL, (value *) NULL, (value) NULL };
      pcre2_match_context* new_mcontext = pcre2_match_context_copy(mcontext);

      pcre2_set_callout(new_mcontext, (int (*)(pcre2_callout_block_8*, void*))&pcre2_callout_handler, &cod);

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

        if (is_dfa) {
          workspace_len = Wosize_val(v_workspace);
          workspace = caml_stat_alloc(sizeof(int) * workspace_len);
          ret =
            pcre2_dfa_match(code, subj, len, pos, v_opt, match_data, new_mcontext,
                (int *) &Field(v_workspace, 0), workspace_len);
        } else
          ret =
            pcre2_match(code, subj, len, pos, v_opt, match_data, new_mcontext);

        caml_stat_free(subj);
      End_roots();

      pcre2_match_context_free(new_mcontext);
      size_t* ovec = pcre2_get_ovector_pointer(match_data);
      if (ret < 0) {
        if (is_dfa) caml_stat_free(workspace);
        pcre2_match_data_free(match_data);
        if (ret == PCRE2_ERROR_CALLOUT) caml_raise(cod.v_exn);
        else handle_match_error("pcre2_match_stub(callout)", ret);
      } else {
        handle_pcre2_match_result(ovec, v_ovec, ovec_len, subj_start, ret);
        if (is_dfa) {
          caml_int_ptr ocaml_workspace_dst =
            (caml_int_ptr) &Field(v_workspace, 0);
          const int *workspace_src = workspace;
          const int *workspace_src_stop = workspace + workspace_len;
          while (workspace_src != workspace_src_stop) {
            *ocaml_workspace_dst = *workspace_src;
            ocaml_workspace_dst++;
            workspace_src++;
          }
          caml_stat_free(workspace);
        }
      }
    }
    pcre2_match_data_free(match_data);
  }

  return Val_unit;
}

CAMLprim value pcre2_match_stub(
    int64_t v_opt, value v_rex, intnat v_pos, intnat v_subj_start, value v_subj,
    value v_ovec, value v_maybe_cof)
{
  return pcre2_match_stub0(v_opt, v_rex, v_pos, v_subj_start, v_subj,
                         v_ovec, v_maybe_cof, (value) NULL);
}

/* Byte-code hook for pcre2_match_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre2_match_stub_bc(value *argv, int __unused argn)
{
  return
    pcre2_match_stub0(
        Long_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
        argv[4], argv[5], argv[6], (value) NULL);
}

/* Byte-code hook for pcre2_dfa_match_stub
   Needed, because there are more than 5 arguments */
CAMLprim value pcre2_dfa_match_stub_bc(value *argv, int __unused argn)
{
  return
    pcre2_match_stub0(
        Long_val(argv[0]), argv[1], Int_val(argv[2]), Int_val(argv[3]),
        argv[4], argv[5], argv[6], argv[7]);
}

static struct custom_operations tables_ops = {
  "pcre2_ocaml_tables",
  pcre2_dealloc_tables,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

/* Generates a new set of chartables for the current locale (see man
   page of PCRE */
CAMLprim value pcre2_maketables_stub(value __unused v_unit)
{
  /* GC will do a full cycle every 1_000_000 table set allocations (one
     table set consumes 864 bytes -> maximum of 864_000_000 bytes unreclaimed
     table sets) */
  const value v_tables =
    caml_alloc_custom(
      &tables_ops, sizeof(struct pcre2_ocaml_tables), 1, 1000000);
  set_tables(v_tables, pcre2_maketables(NULL));
  return v_tables;
}

/* Wraps around the isspace-function */
CAMLprim value pcre2_isspace_stub(value v_c)
{
  return Val_bool(isspace(Int_val(v_c)));
}


/* Returns number of substring associated with a name */

CAMLprim intnat pcre2_substring_number_from_name_stub(value v_rex, value v_name)
{
  const int ret = pcre2_substring_number_from_name(get_rex(v_rex), (PCRE2_SPTR)String_val(v_name));
  if (ret == PCRE2_ERROR_NOSUBSTRING)
    caml_invalid_argument("Named string not found");

  return ret;
}

CAMLprim value pcre2_substring_number_from_name_stub_bc(value v_rex, value v_name)
{
  return Val_int(pcre2_substring_number_from_name_stub(v_rex, v_name));
}


/* Returns array of names of named substrings in a regexp */
CAMLprim value pcre2_names_stub(value v_rex)
{
  CAMLparam1(v_rex);
  CAMLlocal1(v_res);
  uint32_t name_count;
  uint32_t entry_size;
  const char *tbl_ptr;
  uint32_t i;

  int ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMECOUNT, &name_count);
  if (ret != 0) raise_internal_error("pcre2_names_stub: namecount");

  ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMEENTRYSIZE, &entry_size);
  if (ret != 0) raise_internal_error("pcre2_names_stub: nameentrysize");

  ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMETABLE, &tbl_ptr);
  if (ret != 0) raise_internal_error("pcre2_names_stub: nametable");

  v_res = caml_alloc(name_count, 0);

  for (i = 0; i < name_count; ++i) {
    value v_name = caml_copy_string(tbl_ptr + 2);
    Store_field(v_res, i, v_name);
    tbl_ptr += entry_size;
  }

  CAMLreturn(v_res);
}

/* Generic stub for getting integer results from pcre2_config */
static inline int pcre2_config_int(int what)
{
  int ret;
  pcre2_config(what, (void *) &ret);
  return ret;
}

/* Generic stub for getting long integer results from pcre2_config */
static inline long pcre2_config_long(int what)
{
  long ret;
  pcre2_config(what, (void *) &ret);
  return ret;
}


/* Some stubs for config-functions */

/* Makes OCaml-string from PCRE-version */
CAMLprim value pcre2_version_stub(value __unused v_unit) {
  CAMLparam1(v_unit);
  CAMLlocal1(v_version);
  v_version = caml_alloc_string(32);

  pcre2_config(PCRE2_CONFIG_VERSION, (void *)String_val(v_version));

  CAMLreturn(v_version);
}

/* Returns boolean indicating unicode support */
CAMLprim value pcre2_config_unicode_stub(value __unused v_unit)
{ return Val_bool(pcre2_config_int(PCRE2_CONFIG_UNICODE)); }


/* Returns character used as newline */
CAMLprim value pcre2_config_newline_stub(value __unused v_unit)
{ return Val_int(pcre2_config_int(PCRE2_CONFIG_NEWLINE)); }


/* Returns number of bytes used for internal linkage of regular expressions */

CAMLprim intnat pcre2_config_link_size_stub(value __unused v_unit)
{ return pcre2_config_int(PCRE2_CONFIG_LINKSIZE); }

CAMLprim value pcre2_config_link_size_stub_bc(value v_unit)
{ return Val_int(pcre2_config_link_size_stub(v_unit)); }


/* Returns default limit for calls to internal matching function */

CAMLprim intnat pcre2_config_match_limit_stub(value __unused v_unit)
{ return pcre2_config_long(PCRE2_CONFIG_MATCHLIMIT); }

CAMLprim value pcre2_config_match_limit_stub_bc(value v_unit)
{ return Val_int(pcre2_config_match_limit_stub(v_unit)); }


/* Returns default limit for depth of nested backtracking  */

CAMLprim intnat pcre2_config_depth_limit_stub(value __unused v_unit)
{ return pcre2_config_long(PCRE2_CONFIG_DEPTHLIMIT); }

CAMLprim value pcre2_config_depth_limit_stub_bc(value v_unit)
{ return Val_int(pcre2_config_depth_limit_stub(v_unit)); }


/* Returns boolean indicating use of stack recursion */
CAMLprim intnat pcre2_config_stackrecurse_stub(value __unused v_unit)
{ return Val_bool(pcre2_config_int(PCRE2_CONFIG_STACKRECURSE)); }
