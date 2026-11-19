/*
	Starts a process in a given working directory, without changing the one
	of this process (shared by all its threads and domains) and without a
	shell. Standard handles and program lookup follow Unix.create_process.
*/
#ifndef _WIN32
/* for posix_spawn_file_actions_addchdir_np */
#define _GNU_SOURCE
#endif
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

CAMLprim value process_spawn_in_supported(value unit) {
	return Val_true;
}

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

/* [argv] is not used: Windows takes the command line [cmdline]. */
CAMLprim value process_spawn_in_native(value cwd, value prog, value argv, value cmdline, value fd0, value fd1, value fd2) {
	CAMLparam5(cwd, prog, argv, cmdline, fd0);
	CAMLxparam2(fd1, fd2);
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

#include <errno.h>
#include <signal.h>
#include <spawn.h>
#include <unistd.h>

#if defined(__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 29))
#define HAS_SPAWN_CHDIR
#elif defined(__APPLE__)
#include <AvailabilityMacros.h>
#if defined(MAC_OS_X_VERSION_MIN_REQUIRED) && MAC_OS_X_VERSION_MIN_REQUIRED >= 101500
#define HAS_SPAWN_CHDIR
#endif
#endif

#ifdef __APPLE__
#include <crt_externs.h>
#define spawn_environ (*_NSGetEnviron())
#else
extern char **environ;
#define spawn_environ environ
#endif

CAMLprim value process_spawn_in_supported(value unit) {
#ifdef HAS_SPAWN_CHDIR
	return Val_true;
#else
	return Val_false;
#endif
}

/* [cmdline] is not used: the arguments are [argv]. */
CAMLprim value process_spawn_in_native(value cwd, value prog, value argv, value cmdline, value fd0, value fd1, value fd2) {
	CAMLparam5(cwd, prog, argv, cmdline, fd0);
	CAMLxparam2(fd1, fd2);
#ifdef HAS_SPAWN_CHDIR
	mlsize_t i, n = Wosize_val(argv);
	char **args = caml_stat_alloc((n + 1) * sizeof(char *));
	char *cprog = caml_stat_strdup(String_val(prog));
	char *ccwd = caml_stat_strdup(String_val(cwd));
	posix_spawn_file_actions_t actions;
	posix_spawnattr_t attr;
	sigset_t mask, defaults;
	pid_t pid = 0;
	int r;
	for (i = 0; i < n; i++)
		args[i] = caml_stat_strdup(String_val(Field(argv, i)));
	args[n] = NULL;
	posix_spawn_file_actions_init(&actions);
	posix_spawnattr_init(&attr);
	/* the child starts with no blocked signal and the default SIGPIPE action */
	sigemptyset(&mask);
	sigemptyset(&defaults);
	sigaddset(&defaults, SIGPIPE);
	r = posix_spawnattr_setsigmask(&attr, &mask);
	if (r == 0) r = posix_spawnattr_setsigdefault(&attr, &defaults);
	if (r == 0) r = posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF);
	if (r == 0) r = posix_spawn_file_actions_adddup2(&actions, Int_val(fd0), 0);
	if (r == 0) r = posix_spawn_file_actions_adddup2(&actions, Int_val(fd1), 1);
	if (r == 0) r = posix_spawn_file_actions_adddup2(&actions, Int_val(fd2), 2);
	if (r == 0) r = posix_spawn_file_actions_addchdir_np(&actions, ccwd);
	if (r == 0) {
		caml_enter_blocking_section();
		r = posix_spawnp(&pid, cprog, &actions, &attr, args, spawn_environ);
		caml_leave_blocking_section();
	}
	posix_spawn_file_actions_destroy(&actions);
	posix_spawnattr_destroy(&attr);
	for (i = 0; i < n; i++)
		caml_stat_free(args[i]);
	caml_stat_free(args);
	caml_stat_free(cprog);
	caml_stat_free(ccwd);
	if (r != 0)
		caml_unix_error(r, "posix_spawn", prog);
	CAMLreturn(Val_int(pid));
#else
	caml_failwith("process_spawn_in: not supported on this system");
#endif
}

#endif

CAMLprim value process_spawn_in_bytecode(value *argv, int argn) {
	return process_spawn_in_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}
