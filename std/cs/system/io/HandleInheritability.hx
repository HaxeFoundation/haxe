package cs.system.io;

/** Specifies whether the underlying handle is inheritable by child processes. */
@:native("System.IO.HandleInheritability")
extern enum HandleInheritability {
	Inheritable;
	None;
}
