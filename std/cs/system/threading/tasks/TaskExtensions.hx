package cs.system.threading.tasks;

/** Provides a set of static (Shared in Visual Basic) methods for working with specific kinds of  instances. */
@:native("System.Threading.Tasks.TaskExtensions")
extern class TaskExtensions {
	@:overload(function(task:cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	/**
	 * Creates a proxy  that represents the asynchronous operation of a .
	 * @param task The  (C#) or  (Visual Basic) to unwrap.
	 * @return A Task that represents the asynchronous operation of the provided .
	 */
	static function Unwrap<TResult>(task:cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<TResult>;
}
