package haxe.coro.schedulers;

/**
	Classes that implement this interface can be passed to instances of
	`haxe.coro.schedulers.Scheduler`.
**/
interface IScheduleObject {
	/**
		This function is invoked when a scheduled object is run.
	**/
	function onSchedule():Void;
}