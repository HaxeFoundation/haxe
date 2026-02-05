package cs.system.threading;

/** The exception that is thrown when a call is made to the  method. This class cannot be inherited. */
@:native("System.Threading.ThreadAbortException")
extern class ThreadAbortException extends cs.system.SystemException {
	/**
	 * Gets an object that contains application-specific information related to the
	 * thread abort.
	 * @return An object containing application-specific information.
	 */
	var ExceptionState(default, never):Dynamic;
}
