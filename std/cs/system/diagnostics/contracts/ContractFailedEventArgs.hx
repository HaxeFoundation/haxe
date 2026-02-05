package cs.system.diagnostics.contracts;

/** Provides methods and data for the  event. */
@:native("System.Diagnostics.Contracts.ContractFailedEventArgs")
extern class ContractFailedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the condition for the failure of the contract.
	 * @return The condition for the failure.
	 */
	var Condition(default, never):String;
	/**
	 * Gets the type of contract that failed.
	 * @return One of the enumeration values that specifies the type of contract that
	 * failed.
	 */
	var FailureKind(default, never):cs.system.diagnostics.contracts.ContractFailureKind;
	/**
	 * Indicates whether the  event has been handled.
	 * @return if the event has been handled; otherwise, .
	 */
	var Handled(default, never):Bool;
	/**
	 * Gets the message that describes the  event.
	 * @return The message that describes the event.
	 */
	var Message(default, never):String;
	/**
	 * Gets the original exception that caused the  event.
	 * @return The exception that caused the event.
	 */
	var OriginalException(default, never):cs.system.Exception;
	/**
	 * Indicates whether the code contract escalation policy should be applied.
	 * @return to apply the escalation policy; otherwise, . The default is .
	 */
	var Unwind(default, never):Bool;
	function new(failureKind:cs.system.diagnostics.contracts.ContractFailureKind, message:String, condition:String, originalException:cs.system.Exception):Void;
	/** Sets the  property to . */
	function SetHandled():Void;
	/** Sets the  property to . */
	function SetUnwind():Void;
}
