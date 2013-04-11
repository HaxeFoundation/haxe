package rust.task;

/** A handle to a scheduler */
@:native("Scheduler") extern enum Scheduler {
	SchedulerHandle(id:Int);
}