package cs.system;

/** Identifies the operating system, or platform, supported by an assembly. */
@:native("System.PlatformID")
extern enum PlatformID {
	MacOSX;
	Unix;
	Win32NT;
	Win32S;
	Win32Windows;
	WinCE;
	Xbox;
}
