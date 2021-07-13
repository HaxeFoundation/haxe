package asys.native.system;

/**
	Signal codes as described in signal(7) man page.

	TODO:
	Docs below are copy-pasted from `man 7 signal`.
	Rewrite to avoid legal issues.
**/
extern enum abstract PosixSignalCode(Int) from Int to Int {
	/** Hangup detected on controlling terminal or death of controlling process */
	var SIGHUP;
	/** Interrupt from keyboard */
	var SIGINT;
	/** Quit from keyboard */
	var SIGQUIT;
	/** Illegal Instruction */
	var SIGILL;
	/** Abort signal from abort(3) */
	var SIGABRT;
	/** Floating-point exception */
	var SIGFPE;
	/** Kill signal */
	var SIGKILL;
	/** Invalid memory reference */
	var SIGSEGV;
	/** Broken pipe: write to pipe with no readers */
	var SIGPIPE;
	/** Timer signal from alarm(2) */
	var SIGALRM;
	/** Termination signal */
	var SIGTERM;
	/** User-defined signal 1 */
	var SIGUSR1;
	/** User-defined signal 2 */
	var SIGUSR2;
	/** Child stopped or terminated */
	var SIGCHLD;
	/** Continue if stopped */
	var SIGCONT;
	/** Stop process */
	var SIGSTOP;
	/** Stop typed at terminal */
	var SIGTSTP;
	/** Terminal input for background process */
	var SIGTTIN;
	/** Terminal output for background process */
	var SIGTTOU;
	/** Bus error (bad memory access) */
	var SIGBUS;
	/** Pollable event (Sys V) */
	var SIGPOLL;
	/** Profiling timer expired */
	var SIGPROF;
	/** Bad system call (SVr4) */
	var SIGSYS;
	/** Trace/breakpoint trap */
	var SIGTRAP;
	/** Urgent condition on socket (4.2BSD) */
	var SIGURG;
	/** Virtual alarm clock (4.2BSD) */
	var SIGVTALRM;
	/** CPU time limit exceeded (4.2BSD); */
	var SIGXCPU;
	/** File size limit exceeded (4.2BSD) */
	var SIGXFSZ;
}