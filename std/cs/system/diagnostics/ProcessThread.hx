package cs.system.diagnostics;

/** Represents an operating system process thread. */
@:native("System.Diagnostics.ProcessThread")
extern class ProcessThread extends cs.system.componentmodel.Component {
	/**
	 * Gets the base priority of the thread.
	 * @return The base priority of the thread, which the operating system computes by
	 * combining the process priority class with the priority level of the associated
	 * thread.
	 */
	var BasePriority(default, never):Int;
	/**
	 * Gets the current priority of the thread.
	 * @return The current priority of the thread, which may deviate from the base
	 * priority based on how the operating system is scheduling the thread. The
	 * priority may be temporarily boosted for an active thread.
	 */
	var CurrentPriority(default, never):Int;
	/**
	 * Gets the unique identifier of the thread.
	 * @return The unique identifier associated with a specific thread.
	 */
	var Id(default, never):Int;
	/**
	 * Sets the preferred processor for this thread to run on.
	 * @return The preferred processor for the thread, used when the system schedules
	 * threads, to determine which processor to run the thread on.
	 */
	var IdealProcessor(never, default):Int;
	/**
	 * Gets or sets a value indicating whether the operating system should temporarily
	 * boost the priority of the associated thread whenever the main window of the
	 * thread's process receives the focus.
	 * @return to boost the thread's priority when the user interacts with the
	 * process's interface; otherwise, . The default is .
	 */
	var PriorityBoostEnabled(default, default):Bool;
	/**
	 * Gets or sets the priority level of the thread.
	 * @return One of the  values, specifying a range that bounds the thread's
	 * priority.
	 */
	var PriorityLevel(default, default):cs.system.diagnostics.ThreadPriorityLevel;
	/**
	 * Gets the amount of time that the thread has spent running code inside the
	 * operating system core.
	 * @return A  indicating the amount of time that the thread has spent running code
	 * inside the operating system core.
	 */
	var PrivilegedProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Sets the processors on which the associated thread can run.
	 * @return An  that points to a set of bits, each of which represents a processor
	 * that the thread can run on.
	 */
	var ProcessorAffinity(never, default):cs.system.IntPtr;
	/**
	 * Gets the memory address of the function that the operating system called that
	 * started this thread.
	 * @return The thread's starting address, which points to the application-defined
	 * function that the thread executes.
	 */
	var StartAddress(default, never):cs.system.IntPtr;
	/**
	 * Gets the time that the operating system started the thread.
	 * @return A  representing the time that was on the system when the operating
	 * system started the thread.
	 */
	var StartTime(default, never):cs.system.DateTime;
	/**
	 * Gets the current state of this thread.
	 * @return A  that indicates the thread's execution, for example, running, waiting,
	 * or terminated.
	 */
	var ThreadState(default, never):cs.system.diagnostics.ThreadState;
	/**
	 * Gets the total amount of time that this thread has spent using the processor.
	 * @return A  that indicates the amount of time that the thread has had control of
	 * the processor.
	 */
	var TotalProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the amount of time that the associated thread has spent running code inside
	 * the application.
	 * @return A  indicating the amount of time that the thread has spent running code
	 * inside the application, as opposed to inside the operating system core.
	 */
	var UserProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the reason that the thread is waiting.
	 * @return A  representing the reason that the thread is in the wait state.
	 */
	var WaitReason(default, never):cs.system.diagnostics.ThreadWaitReason;
	/** Resets the ideal processor for this thread to indicate that there is no single ideal processor. In other words, so that any processor is ideal. */
	function ResetIdealProcessor():Void;
}
