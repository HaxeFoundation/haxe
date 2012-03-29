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
#	include <sys/time.h>
#	include <sys/times.h>
#	include <caml/memory.h>
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

#ifndef CLK_TCK
#	define CLK_TCK	100
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

CAMLprim value zlib_inflate_init(value wbits) {
	value z = zlib_new_stream();
	if( inflateInit2(zval(z),Int_val(wbits)) != Z_OK )
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
	value cpath;
	char *path = realpath(String_val(f),NULL);
	if( path == NULL )
		failwith("get_full_path");
	cpath = caml_copy_string(path);
	free(path);
	return cpath;
#endif
}

CAMLprim value get_real_path( value path ) {
#ifdef _WIN32
	value path2 = caml_copy_string(String_val(path));
	char *cur = String_val(path2);
	if( cur[0] == '\\' && cur[1] == '\\' ) {
		cur = strchr(cur,'\\');
		if( cur != NULL ) cur++;
	} else if( cur[0] != 0 && cur[1] == ':' ) {
		char c = cur[0];
		if( c >= 'a' && c <= 'z' )
			cur[0] = c - 'a' + 'A';
		cur += 2;
		if( cur[0] == '\\' )
			cur++;
	}
	while( cur ) {
		char *next = strchr(cur,'\\');
		SHFILEINFOA infos;
		if( next != NULL )
			*next = 0;
		else if( *cur == 0 )
			break;
		if( SHGetFileInfoA( String_val(path2), 0, &infos, sizeof(infos), SHGFI_DISPLAYNAME ) != 0 )
			memcpy(cur,infos.szDisplayName,strlen(infos.szDisplayName)+1);
		if( next != NULL ) {
			*next = '\\';
			cur = next + 1;
		} else
			cur = NULL;
	}
	return path2;
#else
	return path;
#endif
}

CAMLprim value sys_time() {
#ifdef _WIN32
#define EPOCH_DIFF	(134774*24*60*60.0)
	static LARGE_INTEGER freq;
	static int freq_init = -1;
	LARGE_INTEGER counter;
	if( freq_init == -1 )
		freq_init = QueryPerformanceFrequency(&freq);
	if( !freq_init || !QueryPerformanceCounter(&counter) ) {
		SYSTEMTIME t;
		FILETIME ft;
		ULARGE_INTEGER ui;
		GetSystemTime(&t);
		if( !SystemTimeToFileTime(&t,&ft) )
			failwith("sys_cpu_time");
		ui.LowPart = ft.dwLowDateTime;
		ui.HighPart = ft.dwHighDateTime;
		return caml_copy_double( ((double)ui.QuadPart) / 10000000.0 - EPOCH_DIFF );
	}
	return caml_copy_double( ((double)counter.QuadPart) / ((double)freq.QuadPart) );
#else
	struct tms t;
	times(&t);
	return caml_copy_double( ((double)(t.tms_utime + t.tms_stime)) / CLK_TCK );
#endif
}
