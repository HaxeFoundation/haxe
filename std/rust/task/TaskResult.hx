package rust.task;

/** Indicates the manner in which a task exited. */
@:native("task.TaskResult") extern enum TaskResult {
	Success;
	Failure;
}