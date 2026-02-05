package cs.system.threading;

/** The exception that is thrown when a failure occurs in a managed thread after the underlying operating system thread has been started, but before the thread is ready to execute user code. */
@:native("System.Threading.ThreadStartException")
extern class ThreadStartException extends cs.system.SystemException {
}
