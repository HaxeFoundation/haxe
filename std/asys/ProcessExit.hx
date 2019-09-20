package asys;

/**
	Represents how a process exited.
**/
typedef ProcessExit = {
	/**
		Exit code of the process. Non-zero values usually indicate an error.
		Specific meanings of exit codes differ from program to program.
	**/
	var code:Int;
	/**
		Signal that cause the process to exit, or zero if none.
	**/
	var signal:Int;
};
