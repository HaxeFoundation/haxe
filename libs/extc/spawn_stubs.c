/*
	Windows only: starts a process in a given working directory, without
	changing the one of this process (shared by all its threads and domains).

	This is Unix.create_process (OCaml's win32unix/createprocess.c) with the
	lpCurrentDirectory argument of CreateProcess, which the Unix library does
	not expose. It needs CAML_INTERNALS for caml_search_exe_in_path and can go
	once Unix.create_process takes a working directory.
*/
#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>

#ifdef _WIN32

#include <windows.h>

/* As Unix.create_process: without a console, the process gets a hidden one. */
static int has_console(void) {
	HANDLE h = CreateFileW(L"CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (h == INVALID_HANDLE_VALUE)
		return 0;
	CloseHandle(h);
	return 1;
}

/* An inheritable copy of [h], or INVALID_HANDLE_VALUE if [h] is not a handle. */
static BOOL inheritable(HANDLE h, HANDLE *copy) {
	HANDLE self = GetCurrentProcess();
	*copy = INVALID_HANDLE_VALUE;
	if (h == NULL || h == INVALID_HANDLE_VALUE)
		return TRUE;
	return DuplicateHandle(self, h, self, copy, 0, TRUE, DUPLICATE_SAME_ACCESS);
}

CAMLprim value process_spawn_in_native(value cwd, value prog, value cmdline, value fd0, value fd1, value fd2) {
	CAMLparam5(cwd, prog, cmdline, fd0, fd1);
	CAMLxparam1(fd2);
	STARTUPINFOW si;
	PROCESS_INFORMATION pi;
	DWORD flags = CREATE_UNICODE_ENVIRONMENT;
	DWORD err = ERROR_SUCCESS;
	wchar_t *wprog = caml_stat_strdup_to_utf16(String_val(prog));
	wchar_t *exe = caml_search_exe_in_path(wprog);
	wchar_t *wcmdline = caml_stat_strdup_to_utf16(String_val(cmdline));
	wchar_t *wcwd = caml_stat_strdup_to_utf16(String_val(cwd));
	caml_stat_free(wprog);
	ZeroMemory(&si, sizeof(si));
	ZeroMemory(&pi, sizeof(pi));
	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESTDHANDLES;
	if (!inheritable(Handle_val(fd0), &si.hStdInput)
		|| !inheritable(Handle_val(fd1), &si.hStdOutput)
		|| !inheritable(Handle_val(fd2), &si.hStdError))
		err = GetLastError();
	if (err == ERROR_SUCCESS) {
		if (!has_console()) {
			flags |= CREATE_NEW_CONSOLE;
			si.dwFlags |= STARTF_USESHOWWINDOW;
			si.wShowWindow = SW_HIDE;
		}
		if (CreateProcessW(exe, wcmdline, NULL, NULL, TRUE, flags, NULL, wcwd, &si, &pi))
			CloseHandle(pi.hThread);
		else
			err = GetLastError();
	}
	if (si.hStdInput != INVALID_HANDLE_VALUE) CloseHandle(si.hStdInput);
	if (si.hStdOutput != INVALID_HANDLE_VALUE) CloseHandle(si.hStdOutput);
	if (si.hStdError != INVALID_HANDLE_VALUE) CloseHandle(si.hStdError);
	caml_stat_free(exe);
	caml_stat_free(wcmdline);
	caml_stat_free(wcwd);
	if (err != ERROR_SUCCESS) {
		caml_win32_maperr(err);
		caml_uerror("create_process", prog);
	}
	/* a process handle, as the pid of Unix.create_process on Windows */
	CAMLreturn(Val_long((intnat)pi.hProcess));
}

#else

/* Elsewhere /bin/sh changes the directory (see process.ml). */
CAMLprim value process_spawn_in_native(value cwd, value prog, value cmdline, value fd0, value fd1, value fd2) {
	caml_failwith("process_spawn_in: Windows only");
}

#endif

CAMLprim value process_spawn_in_bytecode(value *argv, int argn) {
	return process_spawn_in_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
