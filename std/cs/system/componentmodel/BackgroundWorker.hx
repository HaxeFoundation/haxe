package cs.system.componentmodel;

/** Executes an operation on a separate thread. */
@:native("System.ComponentModel.BackgroundWorker")
extern class BackgroundWorker extends cs.system.componentmodel.Component {
	/**
	 * Gets a value indicating whether the application has requested cancellation of a
	 * background operation.
	 * @return if the application has requested cancellation of a background operation;
	 * otherwise, . The default is .
	 */
	var CancellationPending(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is running an asynchronous operation.
	 * @return , if the  is running an asynchronous operation; otherwise, .
	 */
	var IsBusy(default, never):Bool;
	/**
	 * Gets or sets a value indicating whether the  can report progress updates.
	 * @return if the  supports progress updates; otherwise . The default is .
	 */
	var WorkerReportsProgress(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether the  supports asynchronous cancellation.
	 * @return if the  supports cancellation; otherwise . The default is .
	 */
	var WorkerSupportsCancellation(default, default):Bool;
	function new():Void;
	/** Requests cancellation of a pending background operation. */
	function CancelAsync():Void;
	@:overload(function(percentProgress:Int):Void {})
	/**
	 * Raises the  event.
	 * @param percentProgress The percentage, from 0 to 100, of the background
	 * operation that is complete.
	 */
	function ReportProgress(percentProgress:Int, userState:Dynamic):Void;
	@:overload(function():Void {})
	/** Starts execution of a background operation. */
	function RunWorkerAsync(argument:Dynamic):Void;
}
