package cs.system;

/** Identifies the operating system, or platform, supported by an assembly. */
@:native("System.PlatformID")
extern enum abstract PlatformID(Int) {
	var MacOSX = 6;
	var Unix = 4;
	var Win32NT = 2;
	var Win32S = 0;
	var Win32Windows = 1;
	var WinCE = 3;
	var Xbox = 5;
}
