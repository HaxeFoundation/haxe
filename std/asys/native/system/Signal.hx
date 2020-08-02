package asys.native.system;

/**
	Signals for inter-process communication.
	Signals are sent by operating system or by a process to another process.
	Also, a process can signal itself.

	TODO: any other signals to have own constructors here?
**/
enum Signal {
	/**
		Terminate a process.
		The process is _not_ immediately killed.
		Instead the process can handle this signal to gracefully shutdown
		(remove temporary files, close socket connections etc).
		POSIX equivalent: SIGTERM
	**/
	Terminate;
	/**
		Immediately terminate a process.
		The process cannot handle this signal.
		That means, for example, temporary files may stay in file system.
		POSIX equivalent: SIGKILL
	**/
	Kill;
	/**
		Interrupt a process.
		The same as pressing "CTRL+C" in a terminal.
		POSIX equivalent: SIGINT
	**/
	Interrupt;
	/**
		_Pause_ a process.
		The process cannot handle this signal.
		POSIX equivalent: SIGSTOP
	**/
	Stop;
	/**
		Continue previously stopped process.
		POSIX equivalent: SIGCONT
	**/
	Resume;
	/**
		Any signal can be specified by its code.
	**/
	SignalCode(code:PosixSignalCode);
}