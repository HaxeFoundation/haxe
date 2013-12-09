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
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <zlib.h>
#ifdef _WIN32
#	include <windows.h>
#	include <conio.h>
#else
#	include <dlfcn.h>
#	include <limits.h>
#	include <unistd.h>
#	include <string.h>
#	include <termios.h>
#	include <stdio.h>
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

CAMLprim value zlib_deflate_init2(value lvl,value wbits) {
	value z = zlib_new_stream();
	if( deflateInit2(zval(z),Int_val(lvl),Z_DEFLATED,Int_val(wbits),8,Z_DEFAULT_STRATEGY) != Z_OK )
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

CAMLprim value zlib_deflate_bound(value zv,value len) {
	return Val_int(deflateBound(zval(zv),Int_val(len)));
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
	char path[4096];
	if( realpath(String_val(f),path) == NULL )
		failwith("get_full_path");
	return caml_copy_string(path);
#endif
}

CAMLprim value get_real_path( value path ) {
#ifdef _WIN32
	// this will ensure the full class path with proper casing
	char tmp[MAX_PATH];
	char out[MAX_PATH];
	if( GetFullPathName(String_val(path),MAX_PATH,out,NULL) == 0 )
		failwith("get_real_path");
	// GetLongPath name will ignore parts that are > 8 chars since it assume they are already "long" (sic)
	// let's first reduce our path to short form before expanding it again
	if( GetShortPathName(out,tmp,MAX_PATH) == 0 )
		failwith("get_real_path");
	if( GetLongPathName(tmp,out,MAX_PATH) == 0 )
		failwith("get_real_path");
	if( out[1] == ':' && out[0] >= 'a' && out[0] <= 'z' )
		out[0] += 'A' - 'a';
	return caml_copy_string(out);
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

CAMLprim value sys_getch( value b ) {
#	ifdef _WIN32
	return Val_int( Bool_val(b)?getche():getch() );
#	else
	// took some time to figure out how to do that
	// without relying on ncurses, which clear the
	// terminal on initscr()
	int c;
	struct termios term, old;
	tcgetattr(fileno(stdin), &old);
	term = old;
	cfmakeraw(&term);
	tcsetattr(fileno(stdin), 0, &term);
	c = getchar();
	tcsetattr(fileno(stdin), 0, &old);
	if( Bool_val(b) ) fputc(c,stdout);
	return Val_int(c);
#	endif
}

// --------------- Support for NekoVM Bridge

CAMLprim value sys_dlopen( value lib ) {
#ifdef _WIN32
	return (value)LoadLibrary(String_val(lib));
#else
	return (value)dlopen(String_val(lib),RTLD_LAZY);
#endif
}

CAMLprim value sys_dlsym( value dl, value name ) {
#ifdef _WIN32
	return (value)GetProcAddress((HANDLE)dl,String_val(name));
#else
	return (value)dlsym((void*)dl,String_val(name));
#endif
}

CAMLprim value sys_dlint( value i ) {
	return Int_val(i);
}

CAMLprim value sys_dltoint( value i ) {
	return Val_int((int)i);
}

CAMLprim value sys_dlint32( value i ) {
	return (value)Int32_val(i);
}

typedef value (*c_prim0)();
typedef value (*c_prim1)(value);
typedef value (*c_prim2)(value,value);
typedef value (*c_prim3)(value,value,value);
typedef value (*c_prim4)(value,value,value,value);
typedef value (*c_prim5)(value,value,value,value,value);

CAMLprim value sys_dlcall0( value f ) {
	return ((c_prim0)f)();
}

CAMLprim value sys_dlcall1( value f, value a ) {
	return ((c_prim1)f)(a);
}

CAMLprim value sys_dlcall2( value f, value a, value b ) {
	return ((c_prim2)f)(a,b);
}

CAMLprim value sys_dlcall3( value f, value a, value b, value c ) {
	return ((c_prim3)f)(a,b,c);
}

CAMLprim value sys_dlcall4( value f, value a, value b, value c, value d ) {
	return ((c_prim4)f)(a,b,c,d);
}

CAMLprim value sys_dlcall5( value f, value a, value b, value c, value d, value e ) {
	return ((c_prim5)f)(a,b,c,d,e);
}

CAMLprim value sys_dlcall5_bc( value *args, int nargs ) {
	return ((c_prim5)args[0])(args[1],args[2],args[3],args[4],args[5]);
}

CAMLprim value sys_dladdr( value v, value a ) {
	return (value)((char*)v + Int_val(a));
}

CAMLprim value sys_dlptr( value v ) {
	return *((value*)v);
}

CAMLprim value sys_dlsetptr( value p, value v ) {
	*((value*)p) = v;
	return Val_unit;
}

CAMLprim value sys_dlalloc_string( value v ) {
	return caml_copy_string((char*)v);
}

CAMLprim value sys_dlmemcpy( value dst, value src, value len ) {
	memcpy((char*)dst,(char*)src,Int_val(len));
	return Val_unit;
}

static value __callb0( value callb ) {
	return caml_callbackN(callb,0,NULL);
}

static value __callb1( value a, value callb ) {
	return caml_callback(callb,a);
}

static value __callb2( value a, value b, value callb ) {
	return caml_callback2(callb,a,b);
}

static value __callb3( value a, value b, value c, value callb ) {
	return caml_callback3(callb,a,b,c);
}

CAMLprim value sys_dlcallback( value nargs ) {
	switch( Int_val(nargs) ) {
	case 0:
		return (value)__callb0;
	case 1:
		return (value)__callb1;
	case 2:
		return (value)__callb2;
	case 3:
		return (value)__callb3;
	default:
		failwith("dlcallback(too_many_args)");
	}
	return Val_unit;
}

static value __caml_callb1( value a ) {
	return caml_callback(*caml_named_value("dlcallb1"),a);
}

static value __caml_callb2( value a, value b ) {
	return caml_callback2(*caml_named_value("dlcallb2"),a,b);
}

CAMLprim value sys_dlcaml_callback( value nargs ) {
	switch( Int_val(nargs) ) {
	case 1:
		return (value)__caml_callb1;
	case 2:
		return (value)__caml_callb2;
	default:
		failwith("sys_dlcaml_callback(too_many_args)");
	}
	return Val_unit;
}
