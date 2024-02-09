package eval.luv;

/**
	Process ids.

	@see https://aantron.github.io/luv/luv/Luv/Pid
**/
extern class Pid {
	/**
		Evaluates to the pid of the current process.
	**/
	static function getPid():Int;

	/**
		Evaluates to the pid of the parent process.
	**/
	static function getPPid():Int;
}