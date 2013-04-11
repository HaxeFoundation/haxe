package rust.task;

/** A handle to a task */
@:native("task.Task") extern enum TaskHandle {
	TaskHandle(id:Int);
}