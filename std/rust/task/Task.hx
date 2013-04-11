package rust.task;
@:native("task") extern class Task {
	/** Creates and executes a new child task */
	public static function spawn(fn:Void->Void):Void;
	/** Creates a child task unlinked from the current one. If either this task or the child task fails, the other will not be killed. */
	public static function spawn_supervised(fn:Void->Void):Void;
	/** Creates a child task unlinked from the current one. If either this task or the child task fails, the other will not be killed. */
	public static function spawn_unlinked(fn:Void->Void):Void;
	/** Runs a task, while transfering ownership of one argument to the child. */
	public static function spawn_with<A>(arg:A>, fn:NativeObject<A>-Void):Void;
	/** Generate the base configuration for spawning a task, off of which more configuration methods can be chained. */
	public static function task():TaskBuilder;
	/** Execute a function in another task and return either the return value of the function or result.err. */
	public static function try<T>(fn:Void -> T>>):Result<NativeObject<T>, NativeObject<Void;
	/** Yield control to the task scheduler */
	public static function yield():Void;
}