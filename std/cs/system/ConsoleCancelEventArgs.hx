package cs.system;

/** Provides data for the  event. This class cannot be inherited. */
@:native("System.ConsoleCancelEventArgs")
extern class ConsoleCancelEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets a value that indicates whether simultaneously pressing the 
	 * modifier key and the  console key (Ctrl+C) or the Ctrl+Break keys terminates the
	 * current process. The default is , which terminates the current process.
	 * @return if the current process should resume when the event handler concludes; 
	 * if the current process should terminate. The default value is ; the current
	 * process terminates when the event handler returns. If , the current process
	 * continues.
	 */
	var Cancel(default, default):Bool;
	/**
	 * Gets the combination of modifier and console keys that interrupted the current
	 * process.
	 * @return One of the enumeration values that specifies the key combination that
	 * interrupted the current process. There is no default value.
	 */
	var SpecialKey(default, never):cs.system.ConsoleSpecialKey;
}
