package cs.system.io;

/** Specifies whether the underlying handle is inheritable by child processes. */
@:native("System.IO.HandleInheritability")
extern enum abstract HandleInheritability(Int) {
	var Inheritable = 1;
	var None = 0;
}
