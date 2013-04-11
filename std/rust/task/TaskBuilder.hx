package rust.task;

@:native("task.TaskBuilder") extern class TaskBuilder {
	public var opts(default, null):TaskObps;
	public var gen_body(default, null):(Void -> Void) -> Void;
	public var can_not_copy(default, null):NonCopyable;
	public var consumed:Bool;
	/** Decouple the child task's failure from the parent's. If either fails, the other will not be killed. */
	public function unlinked():TaskBuilder;
	/** Unidirectionally link the child task's failure with the parent's. The child's failure will not kill the parent, but the parent's will kill the child. */
	public function supervised():TaskBuilder;
	/** Link the child task's and parent task's failures. If either fails, the other will be killed. */
	public function linked():TaskBuilder;
	/** Configure a custom scheduler mode for the task. */
	public function sched_mode(m:SchedMode>):NativeObject<TaskBuilder;
}