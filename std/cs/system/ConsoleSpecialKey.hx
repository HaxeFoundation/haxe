package cs.system;

/** Specifies combinations of modifier and console keys that can interrupt the current process. */
@:native("System.ConsoleSpecialKey")
extern enum abstract ConsoleSpecialKey(Int) {
	var ControlBreak = 1;
	var ControlC = 0;
}
