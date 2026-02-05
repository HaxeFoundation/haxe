package cs.system.componentmodel;

/** Provides data for the MethodName event. */
@:native("System.ComponentModel.RunWorkerCompletedEventArgs")
extern class RunWorkerCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets a value that represents the result of an asynchronous operation.
	 * @return An  representing the result of an asynchronous operation.
	 */
	var Result(default, never):Dynamic;
	function new(result:Dynamic, error:cs.system.Exception, cancelled:Bool):Void;
}
