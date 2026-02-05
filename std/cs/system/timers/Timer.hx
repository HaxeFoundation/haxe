package cs.system.timers;

/** Generates an event after a set interval, with an option to generate recurring events. */
@:native("System.Timers.Timer")
extern class Timer extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets a Boolean indicating whether the  should raise the  event only once
	 * () or repeatedly ().
	 * @return if the  should raise the  event each time the interval elapses;  if it
	 * should raise the  event only once, after the first time the interval elapses.
	 * The default is .
	 */
	var AutoReset(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether the  should raise the  event.
	 * @return if the  should raise the  event; otherwise, . The default is .
	 */
	var Enabled(default, default):Bool;
	/**
	 * Gets or sets the interval, expressed in milliseconds, at which to raise the 
	 * event.
	 * @return The time, in milliseconds, between  events. The value must be greater
	 * than zero, and less than or equal to . The default is 100 milliseconds.
	 */
	var Interval(default, default):Float;
	/**
	 * Gets or sets the object used to marshal event-handler calls that are issued when
	 * an interval has elapsed.
	 * @return The  representing the object used to marshal the event-handler calls
	 * that are issued when an interval has elapsed. The default is .
	 */
	var SynchronizingObject(default, default):cs.system.componentmodel.ISynchronizeInvoke;
	@:overload(function():Void {})
	function new(interval:Float):Void;
	/** Begins the run-time initialization of a  that is used on a form or by another component. */
	function BeginInit():Void;
	/** Releases the resources used by the . */
	function Close():Void;
	/** Ends the run-time initialization of a  that is used on a form or by another component. */
	function EndInit():Void;
	/** Starts raising the  event by setting  to . */
	function Start():Void;
	/** Stops raising the  event by setting  to . */
	function Stop():Void;
}
