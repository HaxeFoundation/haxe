/*
 *  Extc : C common OCaml bindings
 *  Copyright (c)2004 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <zlib.h>
#ifdef _WIN32
#	include <windows.h>
#else
#	include <limits.h>
#	include <unistd.h>
#endif
#ifdef __APPLE__
#	include <sys/param.h>
#	include <sys/syslimits.h>
#	include <mach-o/dyld.h>
#endif
#ifdef __FreeBSD__
#	include <sys/param.h>
#	include <sys/sysctl.h>
#	include <sys/user.h>
#endif


#define zval(z)		((z_streamp)(z))

value zlib_new_stream() {
	value z = alloc((sizeof(z_stream) + sizeof(value) - 1) / sizeof(value),Abstract_tag);
	z_stream *s = zval(z);
	s->zalloc = NULL;
	s->zfree = NULL;
	s->opaque = NULL;
	s->next_in = NULL;
	s->next_out = NULL;
	return z;
}

CAMLprim value zlib_deflate_init(value lvl) {
	value z = zlib_new_stream();
	if( deflateInit(zval(z),Int_val(lvl)) != Z_OK )
		failwith("zlib_deflate_init");
	return z;
}

CAMLprim value zlib_deflate( value zv, value src, value spos, value slen, value dst, value dpos, value dlen, value flush ) {
	z_streamp z = zval(zv);
	value res;
	int r;

	z->next_in = (Bytef*)(String_val(src) + Int_val(spos));
	z->next_out = (Bytef*)(String_val(dst) + Int_val(dpos));
	z->avail_in = Int_val(slen);
	z->avail_out = Int_val(dlen);
	if( (r = deflate(z,Int_val(flush))) < 0 )
		failwith("zlib_deflate");

	z->next_in = NULL;
	z->next_out = NULL;

	res = alloc_small(3, 0);
	Field(res, 0) = Val_bool(r == Z_STREAM_END);
	Field(res, 1) = Val_int(Int_val(slen) - z->avail_in);
	Field(res, 2) = Val_int(Int_val(dlen) - z->avail_out);
	return res;
}

CAMLprim value zlib_deflate_bytecode(value * arg, int nargs) {
	return zlib_deflate(arg[0],arg[1],arg[2],arg[3],arg[4],arg[5],arg[6],arg[7]);
}

CAMLprim value zlib_deflate_end(value zv) {
	if( deflateEnd(zval(zv)) != 0 )
		failwith("zlib_deflate_end");
	return Val_unit;
}

CAMLprim value zlib_inflate_init() {
	value z = zlib_new_stream();
	if( inflateInit(zval(z)) != Z_OK )
		failwith("zlib_inflate_init");
	return z;
}

CAMLprim value zlib_inflate( value zv, value src, value spos, value slen, value dst, value dpos, value dlen, value flush ) {
	z_streamp z = zval(zv);
	value res;
	int r;

	z->next_in = (Bytef*)(String_val(src) + Int_val(spos));
	z->next_out = (Bytef*)(String_val(dst) + Int_val(dpos));
	z->avail_in = Int_val(slen);
	z->avail_out = Int_val(dlen);
	if( (r = inflate(z,Int_val(flush))) < 0 )
		failwith("zlib_inflate");

	z->next_in = NULL;
	z->next_out = NULL;

	res = alloc_small(3, 0);
	Field(res, 0) = Val_bool(r == Z_STREAM_END);
	Field(res, 1) = Val_int(Int_val(slen) - z->avail_in);
	Field(res, 2) = Val_int(Int_val(dlen) - z->avail_out);
	return res;
}

CAMLprim value zlib_inflate_bytecode(value * arg, int nargs) {
	return zlib_inflate(arg[0],arg[1],arg[2],arg[3],arg[4],arg[5],arg[6],arg[7]);
}

CAMLprim value zlib_inflate_end(value zv) {
	if( inflateEnd(zval(zv)) != 0 )
		failwith("zlib_inflate_end");
	return Val_unit;
}

CAMLprim value executable_path(value u) {
#ifdef _WIN32
	char path[MAX_PATH];
	if( GetModuleFileName(NULL,path,MAX_PATH) == 0 )
		failwith("executable_path");
	return caml_copy_string(path);
#elif __APPLE__
	char path[MAXPATHLEN+1];
	uint32_t path_len = MAXPATHLEN;
	if ( _NSGetExecutablePath(path, &path_len) )
		failwith("executable_path");
	return caml_copy_string(path);
#elif __FreeBSD__
	char path[PATH_MAX];
	int error, name[4];
	size_t len;
	name[0] = CTL_KERN;
	name[1] = KERN_PROC;
	name[2] = KERN_PROC_PATHNAME;
	name[3] = (int)getpid();
	len = sizeof(path);
	error = sysctl(name, 4, path, &len, NULL, 0);
	if( error < 0 )
		failwith("executable_path");
	return caml_copy_string(path);
#else
	const char *p = getenv("_");
	if( p != NULL )
		return caml_copy_string(p);
	{
		char path[200];
		int length = readlink("/proc/self/exe", path, sizeof(path));
		if( length < 0 || length >= 200 )
			failwith("executable_path");
	    path[length] = '\0';
		return caml_copy_string(path);
	}
#endif
}

CAMLprim value get_full_path( value f ) {
#ifdef _WIN32
	char path[MAX_PATH];
	if( GetFullPathName(String_val(f),MAX_PATH,path,NULL) == 0 )
		failwith("get_full_path");
	return caml_copy_string(path);
#else
	char path[PATH_MAX];
	if( realpath(String_val(f),path) == NULL )
		failwith("get_full_path");
	return caml_copy_string(path);
#endif
}
