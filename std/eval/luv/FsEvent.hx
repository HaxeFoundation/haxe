package eval.luv;

enum abstract FsEventType(Int) {
	var RENAME = 0;
	var CHANGE = 1;
}

enum abstract FsEventFlag(Int) {
	var FS_EVENT_WATCH_ENTRY = 0;
	var FS_EVENT_STAT = 1;
	var FS_EVENT_RECURSIVE = 2;
}

/**
	Filesystem events.

	@see https://aantron.github.io/luv/luv/Luv/FS_event
**/
@:using(eval.luv.Handle)
@:coreType abstract FsEvent to Handle {
	/**
		Allocates and initializes an FS event handle.
	**/
	static public function init(loop:Loop):Result<FsEvent>;

	/**
		Starts the handle and watches the given path for changes.
	**/
	public function start(path:NativeString, ?flags:Array<FsEventFlag>, callback:(result:Result<{file:Null<NativeString>,events:Array<FsEventType>}>)->Void):Void;

	/**
		Stops the handle.
	**/
	public function stop():Result<Result.NoData>;

}