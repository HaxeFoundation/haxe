package eval.luv;

enum abstract TtyMode(Int) {
	var NORMAL = 0;
	var RAW = 1;
	var IO = 2;
}

enum abstract VTermState(Int) {
	var SUPPORTED = 0;
	var UNSUPPORTED = 1;
}

/**
	Consoles.

	@see https://aantron.github.io/luv/luv/Luv/Tty
**/
@:using(eval.luv.Handle)
@:using(eval.luv.Stream)
@:coreType abstract Tty to Handle to Stream to Stream.TStream<Tty> {
	/**
		To be called when the program exits.
		Resets TTY settings to default values for the next process to take over.
	**/
	static public function resetMode():Result<Result.NoData>;

	/**
		Controls whether console virtual terminal sequences are processed by libuv
		or console. Useful in particular for enabling ConEmu support of ANSI X3.64
		and Xterm 256 colors. Otherwise Windows10 consoles are usually detected
		automatically.

		This function is only meaningful on Windows systems. On Unix it is silently
		ignored.
	**/
	static public function setVTermState(state:VTermState):Void;

	/**
		Get the current state of whether console virtual terminal sequences are
		handled by libuv or the console.

		This function is not implemented on Unix, where it returns `UVError.UV_ENOTSUP`.
	**/
	static public function getVTermState():Result<VTermState>;

	/**
		Allocates and initializes a TTY handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop, file:File):Result<Tty>;

	/**
		Sets the TTY's mode.
	**/
	public function setMode(mode:TtyMode):Result<Result.NoData>;

	/**
		Retrieves the current window size.
	**/
	public function getWinSize():Result<{width:Int, height:Int}>;
}