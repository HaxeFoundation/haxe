package rust.vm;

/** Indicates the manner in which a task exited. */
@:native("task.TaskResult") extern enum TaskResult {
	Success;
	Failure;
}