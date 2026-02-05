package cs.system.diagnostics;

/** Correlates traces that are part of a logical transaction. */
@:native("System.Diagnostics.CorrelationManager")
extern class CorrelationManager {
	/**
	 * Gets or sets the identity for a global activity.
	 * @return A  structure that identifies the global activity.
	 */
	var ActivityId(default, default):cs.system.Guid;
	/**
	 * Gets the logical operation stack from the call context.
	 * @return A  object that represents the logical operation stack for the call
	 * context.
	 */
	var LogicalOperationStack(default, never):cs.system.collections.Stack;
	@:overload(function():Void {})
	/** Starts a logical operation on a thread. */
	function StartLogicalOperation(operationId:Dynamic):Void;
	/** Stops the current logical operation. */
	function StopLogicalOperation():Void;
}
