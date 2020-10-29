package eval.luv;

import eval.luv.File;

/**
	Filesystem polling.

	@see https://aantron.github.io/luv/luv/Luv/FS_poll
**/
@:using(eval.luv.Handle)
@:coreType abstract FsPoll to Handle {
	/**
		Allocates and initializes an FS polling handle.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop):Result<FsPoll>;

	/**
		Starts the handle and polls the given path for changes.

		The default value of `interval` is 2000 (milliseconds).
	**/
	public function start(path:NativeString, ?interval:Int, callback:(result:Result<{previous:FileStat,current:FileStat}>)->Void):Void;

	/**
		Stops the handle.
	**/
	public function stop():Result<Result.NoData>;
}