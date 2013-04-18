package rust.vm;

/** Scheduler modes */
@:native("task.SchedMode") extern enum SchedMode {
	/** Run task on the default scheduler */
	DefaultScheduler;
	/** Run task on the current scheduler */
	CurrentScheduler;
	/** Run task on a specific scheduler */
	ExistingScheduler(s:Scheduler);
	/** Tasks are scheduled on the main OS thread */
	PlatformThread;
}