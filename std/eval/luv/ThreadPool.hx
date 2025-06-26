package eval.luv;

@:forward
abstract ThreadPoolRequest(Request) to Request {}

/**
	Thread pool.

	@see https://aantron.github.io/luv/luv/Luv/Thread_pool
**/
extern class ThreadPool {

	static function createRequest():ThreadPoolRequest;

	/**
		Schedules a function to be called by a thread in the thread pool.

		`work` is the function that will be called in the thread pool.
		`callback` will be called by the `loop` after `work` completes, or
		immediately, in case there is an error scheduling `work`.
	**/
	static function queueWork(loop:Loop, ?request:ThreadPoolRequest, work:()->Void, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Sets thread pool size.

		This function should be called as soon during process startup as possible.
	**/
	static function setSize(size:Int, ?ifNotAlreadySet:Bool):Void;

}