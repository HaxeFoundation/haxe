/*
 *  Extc : C common OCaml bindings
 *  Copyright (c)2004-2017 Nicolas Cannasse
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 */

#include <assert.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>
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
#	include <time.h>
#	include <sys/time.h>
#	include <sys/times.h>
#	include <sys/stat.h>
#	include <caml/memory.h>
#endif
#ifdef __APPLE__
#	include <sys/param.h>
#	include <sys/syslimits.h>
#	include <mach-o/dyld.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#endif
#ifdef __FreeBSD__
#	include <sys/param.h>
#	include <sys/sysctl.h>
#	include <sys/user.h>
#endif

#ifndef CLK_TCK
#	define CLK_TCK	100
#endif

/**
 * Converts an OCaml value to a C pointer for a z_stream.
 *
 * @param v {value} An OCaml value
 * @return {z_streamp} A pointer for a z_stream
 */
#define ZStreamP_val(v) (*((z_streamp *) Data_custom_val(v)))

/**
 * Converts an OCaml `Extc.zflush` value to an allowed flush value for _zlib_.
 *
 * It may raise the following OCaml exception:
 * - Failure: Unknown zflush value.
 *
 * Make sure to update this function when refactoring OCaml's `Extc.zflush` type. The integer value
 * of OCaml's `Extc.zflush` is the 0-based index of the position of the constructor in the type
 * definition.
 *
 * See:
 * https://github.com/HaxeFoundation/haxe-debian/blob/31cb4aaab9f6770d058883a1c5b97e36c8ec5d71/libs/extc/extc.ml#L22
 * https://github.com/madler/zlib/blob/cacf7f1d4e3d44d871b605da3b647f07d718623f/zlib.h#L168
 *
 * @param zflush_val {value} OCaml `Extc.zflush`
 * @return {int} C int representing an allowed flush value for _zlib_
 */
int Zflush_val(value zflush_val) {
	switch (Int_val(zflush_val)) {
		case 0: return Z_NO_FLUSH;
		case 1: return Z_PARTIAL_FLUSH;
		case 2: return Z_SYNC_FLUSH;
		case 3: return Z_FULL_FLUSH;
		case 4: return Z_FINISH;
		// TODO: support Z_BLOCK and Z_TREE
		// TODO: append the received value
		default: caml_failwith("Error in `Zflush_val` (extc_stubs.c): Unknown zflush value");
	}
	assert(0);
}

/**
 * Converts an allowed flush value for _zlib_ to an OCaml `Extc.zflush` value.
 *
 * Make sure to update this function when refactoring OCaml's `Extc.zflush` type. The integer value
 * of OCaml's `Extc.zflush` is the 0-based index of the position of the constructor in the type
 * definition.
 *
 * See:
 * https://github.com/madler/zlib/blob/cacf7f1d4e3d44d871b605da3b647f07d718623f/zlib.h#L168
 * https://github.com/HaxeFoundation/haxe-debian/blob/31cb4aaab9f6770d058883a1c5b97e36c8ec5d71/libs/extc/extc.ml#L22
 *
 * @param {int} C int representing an allowed flush value for _zlib_
 * @return {value} OCaml `Extc.zflush`
 */
value val_Zflush(int zflush) {
	switch (zflush) {
		case Z_NO_FLUSH: return Val_int(0);
		case Z_PARTIAL_FLUSH: return Val_int(1);
		case Z_SYNC_FLUSH: return Val_int(2);
		case Z_FULL_FLUSH: return Val_int(3);
		case Z_FINISH: return Val_int(4);
		// TODO: support Z_BLOCK and Z_TREE
	}
	assert(0);
}

/**
 * Free the memory of the pointer contained in the supplied OCaml value `caml_z_stream_pointer`.
 *
 * @param z_streamp_val {value} An OCaml value containing a z_stream pointer to the memory to free.
 */
void zlib_free_stream(value z_streamp_val) {
	caml_stat_free(ZStreamP_val(z_streamp_val));
	ZStreamP_val(z_streamp_val) = NULL;
}

/**
 * Define the custom operations for a z_stream. This ensures that the memory owned
 * by the z_stream pointer is freed.
 *
 * See:
 * https://github.com/ocaml/ocaml/blob/70d880a41a82aae1ebd428fd38100e8467f8535a/byterun/caml/custom.h#L25
 */
static struct custom_operations zlib_stream_ops = {
	// identifier
	"z_stream_ops",
	// finalize
	&zlib_free_stream,
	// compare
	NULL,
	// hash
	NULL,
	// serialize
	NULL,
	// compare_ext
	NULL
};

/**
 * Create an OCaml value containing a new z_stream pointer.
 *
 * This function may raise the following OCaml exception:
 * - Out_of_memory exception
 *
 * @return {value} An OCaml value containing a new z_stream pointer.
 */
value zlib_new_stream() {
    value z_streamp_val = caml_alloc_custom(&zlib_stream_ops, sizeof(z_streamp), 0, 1);
    ZStreamP_val(z_streamp_val) = caml_stat_alloc(sizeof(z_stream));
    ZStreamP_val(z_streamp_val)->zalloc = NULL;
    ZStreamP_val(z_streamp_val)->zfree = NULL;
    ZStreamP_val(z_streamp_val)->opaque = NULL;
    ZStreamP_val(z_streamp_val)->next_in = NULL;
    ZStreamP_val(z_streamp_val)->next_out = NULL;
    return z_streamp_val;
}

/**
 * OCaml binding for _zlib_'s `deflateInit2` function.
 *
 * This creates a new stream and initializes it for deflate.
 *
 * This function may raise the following OCaml exceptions:
 * - Out_of_memory exception
 * - Failure exception: Invalid parameters
 * - Failure exception: Invalid version
 * - Failure exception: Unknown zlib return code
 *
 * See:
 * https://github.com/madler/zlib/blob/cacf7f1d4e3d44d871b605da3b647f07d718623f/zlib.h#L538
 *
 * @param levelVal {value} OCaml `int`: the compression level, must be in the range 0..9.
 *     0 gives no compression at all, 1 the best speed, 9 the best compression.
 * @param windowBitsVal {value} OCaml `int`: base two logarithm of the window size (size of the
 *     history buffer) used by _zlib_. It should be in the range 9..15 for this version of _zlib_.
 *     It can also be in the range -15..-8 (the absolute value is used) for raw deflate.
 *     Finally, it can be greater than 15 for gzip encoding. See _zlib_'s documentation for
 *     `deflateInit2` for the exact documentation.
 * @return {value} An OCaml value representing the new stream, initialized for deflate.
 */
CAMLprim value zlib_deflate_init2(value level_val, value window_bits_val) {
	int level = Int_val(level_val);
	int window_bits = Int_val(window_bits_val);
	value z_streamp_val = zlib_new_stream();
	z_streamp stream = ZStreamP_val(z_streamp_val);

	int deflate_init2_result = deflateInit2(
		stream,
		level,
		Z_DEFLATED, // method
		window_bits,
		8, // memLevel
		Z_DEFAULT_STRATEGY // strategy
	);

	if (deflate_init2_result == Z_OK) {
		return z_streamp_val;
	}

	switch (deflate_init2_result) {
		case Z_MEM_ERROR:
			caml_raise_out_of_memory();
			break;
		case Z_STREAM_ERROR:
			// TODO: use stream->msg to get _zlib_'s text message
			caml_failwith("Error in `zlib_deflate_init2` (extc_stubs.c): call to `deflateInit2` failed: Z_STREAM_ERROR");
			break;
		case Z_VERSION_ERROR:
			// TODO: use stream->msg to get _zlib_'s text message
			caml_failwith("Error in `zlib_deflate_init2` (extc_stubs.c): call to `deflateInit2` failed: Z_VERSION_ERROR");
			break;
		default:
			caml_failwith("Error in `zlib_deflate_init2` (extc_stubs.c): unknown return code from `deflateInit2`");
	}
	assert(0);
}

/**
 * OCaml binding for _zlib_'s `deflate` function.
 *
 * Compresses as much data as possible, and stops when the input buffer becomes empty or the output
 * buffer becomes full.
 *
 * This function may raise the following OCaml exceptions:
 * - Out_of_memory exception
 * - Failure exception: Invalid parameters
 * - Failure exception: Invalid version
 * - Failure exception: Unknown zlib return code
 *
 * See:
 * https://github.com/madler/zlib/blob/cacf7f1d4e3d44d871b605da3b647f07d718623f/zlib.h#L250
 *
 * @param stream_val {value} OCaml `Extc.zstream`: value containing a z_stream pointer to a deflate
 *     stream.
 * @param src {value} OCaml `bytes`: Source buffer
 * @param spos {value} OCaml `int`: Index of the inclusive start offset of the source.
 * @param slen {value} OCaml `int`: Length of the data to read from the source buffer, from spos.
 * @param dst {value} OCaml `bytes`: Source buffer
 * @param dpos {value} OCaml `int`: Index of the inclusive start offset of the source.
 * @param dlen {value} OCaml `int`: Length of the data to read from the source buffer, from spos.
 * @param flush_val {value} OCaml `Extc.zflush`: Controls the flush logic. See _zlib_'s
 *     documentation.
 * @return {value} OCaml `Extc.reslut`.
 */
CAMLprim value zlib_deflate(value stream_val, value src, value spos, value slen, value dst, value dpos, value dlen, value flush_val) {
	z_streamp stream = ZStreamP_val(stream_val);
	int flush = Zflush_val(flush_val);

	stream->next_in = (Bytef*)(String_val(src) + Int_val(spos));
	stream->next_out = (Bytef*)(String_val(dst) + Int_val(dpos));
	stream->avail_in = Int_val(slen);
	stream->avail_out = Int_val(dlen);

	int deflate_result = deflate(stream, flush);

	if (deflate_result == Z_OK || deflate_result == Z_STREAM_END) {
		stream->next_in = NULL;
		stream->next_out = NULL;
		value zresult = caml_alloc_small(3, 0);
		// z_finish
		Field(zresult, 0) = Val_bool(deflate_result == Z_STREAM_END);
		// z_read
		Field(zresult, 1) = Val_int(Int_val(slen) - stream->avail_in);
		// z_wrote
		Field(zresult, 2) = Val_int(Int_val(dlen) - stream->avail_out);

		return zresult;
	}
	switch (deflate_result) {
		case Z_MEM_ERROR:
			caml_raise_out_of_memory();
			break;
		case Z_STREAM_ERROR:
			// TODO: use stream->msg to get _zlib_'s text message
			caml_failwith("Error in `zlib_deflate` (extc_stubs.c): call to `deflate` failed: Z_STREAM_ERROR");
			break;
		case Z_BUF_ERROR:
			// TODO: use stream->msg to get _zlib_'s text message
			caml_failwith("Error in `zlib_deflate` (extc_stubs.c): call to `deflate` failed: Z_BUF_ERROR");
			break;
		default:
			caml_failwith("Error in `zlib_deflate` (extc_stubs.c): unknown return code from `deflate`");
	}
	assert(0);
}

CAMLprim value zlib_deflate_bytecode(value *arg, int nargs) {
	return zlib_deflate(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7]);
}

CAMLprim value zlib_deflate_end(value zv) {
	if( deflateEnd(ZStreamP_val(zv)) != 0 )
		caml_failwith("zlib_deflate_end");
	return Val_unit;
}

CAMLprim value zlib_inflate_init(value wbits) {
	value z = zlib_new_stream();
	if( inflateInit2(ZStreamP_val(z),Int_val(wbits)) != Z_OK )
		caml_failwith("zlib_inflate_init");
	return z;
}

CAMLprim value zlib_inflate( value zv, value src, value spos, value slen, value dst, value dpos, value dlen, value flush ) {
	z_streamp z = ZStreamP_val(zv);
	value res;
	int r;

	z->next_in = (Bytef*)(String_val(src) + Int_val(spos));
	z->next_out = (Bytef*)(String_val(dst) + Int_val(dpos));
	z->avail_in = Int_val(slen);
	z->avail_out = Int_val(dlen);
	if( (r = inflate(z,Int_val(flush))) < 0 )
		caml_failwith("zlib_inflate");

	z->next_in = NULL;
	z->next_out = NULL;

	res = caml_alloc_small(3, 0);
	Field(res, 0) = Val_bool(r == Z_STREAM_END);
	Field(res, 1) = Val_int(Int_val(slen) - z->avail_in);
	Field(res, 2) = Val_int(Int_val(dlen) - z->avail_out);
	return res;
}

CAMLprim value zlib_inflate_bytecode(value * arg, int nargs) {
	return zlib_inflate(arg[0],arg[1],arg[2],arg[3],arg[4],arg[5],arg[6],arg[7]);
}

CAMLprim value zlib_inflate_end(value zv) {
	if( inflateEnd(ZStreamP_val(zv)) != 0 )
		caml_failwith("zlib_inflate_end");
	return Val_unit;
}

CAMLprim value zlib_deflate_bound(value zv,value len) {
	return Val_int(deflateBound(ZStreamP_val(zv),Int_val(len)));
}

CAMLprim value zlib_crc32( value src, value len ) {
	CAMLparam2(src,len);
	CAMLlocal1(result);
	uLong crc = crc32(0L, (Bytef*)(String_val(src)), Int_val(len));
	result = caml_copy_int32(crc);
	CAMLreturn(result);
}

CAMLprim value executable_path(value u) {
#ifdef _WIN32
	char path[MAX_PATH];
	if( GetModuleFileName(NULL,path,MAX_PATH) == 0 )
		caml_failwith("executable_path");
	return caml_copy_string(path);
#elif __APPLE__
	char path[MAXPATHLEN+1];
	uint32_t path_len = MAXPATHLEN;
	if ( _NSGetExecutablePath(path, &path_len) )
		caml_failwith("executable_path");
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
		caml_failwith("executable_path");
	return caml_copy_string(path);
#else
	char path[PATH_MAX];
	int length = readlink("/proc/self/exe", path, sizeof(path));
	if( length < 0 || length >= PATH_MAX ) {
		const char *p = getenv("_");
		if( p != NULL )
			return caml_copy_string(p);
		else
			caml_failwith("executable_path");
	}
	path[length] = '\0';
	return caml_copy_string(path);
#endif
}

CAMLprim value get_full_path( value f ) {
#ifdef _WIN32
	char path[MAX_PATH];
	if( GetFullPathName(String_val(f),MAX_PATH,path,NULL) == 0 )
		caml_failwith("get_full_path");
	return caml_copy_string(path);
#else
	char path[4096];
	if( realpath(String_val(f),path) == NULL )
		caml_failwith("get_full_path");
	return caml_copy_string(path);
#endif
}

CAMLprim value get_real_path( value path ) {
#ifdef _WIN32
	const char sep = '\\';
	size_t len, i, last;
	WIN32_FIND_DATA data;
	HANDLE handle;
	char out[MAX_PATH];

	// this will ensure the full class path with proper casing
	if( GetFullPathName(String_val(path),MAX_PATH,out,NULL) == 0 )
		caml_failwith("get_real_path");

	len = strlen(out);
	i = 0;

	if (len >= 2 && out[1] == ':') {
		// convert drive letter to uppercase
		if (out[0] >= 'a' && out[0] <= 'z')
			out[0] += 'A' - 'a';
		if (len >= 3 && out[2] == sep)
			i = 3;
		else
			i = 2;
	}

	last = i;

	while (i < len) {
		// skip until separator
		while (i < len && out[i] != sep)
			i++;

		// temporarily strip string to last found component
		out[i] = 0;

		// get actual file/dir name with proper case
		if ((handle = FindFirstFile(out, &data)) != INVALID_HANDLE_VALUE) {
			int klen = strlen(data.cFileName);
			// a ~ was expanded !
			if( klen != i - last ) {
				int d = klen - (i - last);
				memmove(out + i + d, out + i, len - i + 1);
				len += d;
				i += d;
			}
			// replace the component with proper case
			memcpy(out + last, data.cFileName, klen + 1);
			FindClose(handle);
		}

		// if we're not at the end, restore the path
		if (i < len)
			out[i] = sep;

		// advance
		i++;
		last = i;
	}

	return caml_copy_string(out);
#else
	return path;
#endif
}

#ifndef _WIN32
#define TimeSpecToSeconds(ts) (double)ts.tv_sec + (double)ts.tv_nsec / 1000000000.0
#endif

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
			caml_failwith("sys_cpu_time");
		ui.LowPart = ft.dwLowDateTime;
		ui.HighPart = ft.dwHighDateTime;
		return caml_copy_double( ((double)ui.QuadPart) / 10000000.0 - EPOCH_DIFF );
	}
	return caml_copy_double( ((double)counter.QuadPart) / ((double)freq.QuadPart) );
#elif __APPLE__

	uint64_t time;
	uint64_t elapsedNano;
	static mach_timebase_info_data_t sTimebaseInfo;

	time = mach_absolute_time();

	if ( sTimebaseInfo.denom == 0 ) {
		(void) mach_timebase_info(&sTimebaseInfo);
	}

	elapsedNano = time * sTimebaseInfo.numer / sTimebaseInfo.denom;

	return caml_copy_double(time / 1000000000.0);
#elif defined CLOCK_MONOTONIC_RAW
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC_RAW, &t);
	return caml_copy_double(TimeSpecToSeconds(t));
#else
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return caml_copy_double(TimeSpecToSeconds(t));
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

CAMLprim value sys_filetime( value file ) {
#	ifdef _WIN32
	FILETIME fp;
	ULARGE_INTEGER ui;
	HANDLE h = CreateFile(String_val(file),FILE_READ_ATTRIBUTES,FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,NULL,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,NULL);
	if( h == INVALID_HANDLE_VALUE || !GetFileTime(h,NULL,NULL,&fp) ) {
		CloseHandle(h);
		return caml_copy_double(0.);
	}
	CloseHandle(h);
	ui.LowPart = fp.dwLowDateTime;
	ui.HighPart = fp.dwHighDateTime;
	return caml_copy_double( ((double)ui.QuadPart) / 10000000.0 - EPOCH_DIFF );
#	else
	struct stat sbuf;
	if( stat(String_val(file),&sbuf) < 0 )
		return caml_copy_double(0.);
	return caml_copy_double( sbuf.st_mtime );
#	endif
}