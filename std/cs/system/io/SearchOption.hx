package cs.system.io;

/** Specifies whether to search the current directory, or the current directory and all subdirectories. */
@:native("System.IO.SearchOption")
extern enum abstract SearchOption(Int) {
	var AllDirectories = 1;
	var TopDirectoryOnly = 0;
}
