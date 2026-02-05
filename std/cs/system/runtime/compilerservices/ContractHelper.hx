package cs.system.runtime.compilerservices;

/** Provides methods that the binary rewriter uses to handle contract failures. */
@:native("System.Runtime.CompilerServices.ContractHelper")
extern class ContractHelper {
	/**
	 * Used by the binary rewriter to activate the default failure behavior.
	 * @param failureKind One of the enumeration values that specifies the type of
	 * failure.
	 * @param userMessage Additional user information.
	 * @param conditionText The description of the condition that caused the failure.
	 * @param innerException The inner exception that caused the current exception.
	 * @return A null reference ( in Visual Basic) if the event was handled and should
	 * not trigger a failure; otherwise, returns the localized failure message.
	 */
	static function RaiseContractFailedEvent(failureKind:cs.system.diagnostics.contracts.ContractFailureKind, userMessage:String, conditionText:String, innerException:cs.system.Exception):String;
	/**
	 * Triggers the default failure behavior.
	 * @param kind One of the enumeration values that specifies the type of failure.
	 * @param displayMessage The message to display.
	 * @param userMessage Additional user information.
	 * @param conditionText The description of the condition that caused the failure.
	 * @param innerException The inner exception that caused the current exception.
	 */
	static function TriggerFailure(kind:cs.system.diagnostics.contracts.ContractFailureKind, displayMessage:String, userMessage:String, conditionText:String, innerException:cs.system.Exception):Void;
}
