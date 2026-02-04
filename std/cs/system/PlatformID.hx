package cs.system;

@:native("System.PlatformID")
extern enum abstract PlatformID(Int) {
	var Win32S;
	var Win32Windows;
	var Win32NT;
	var WinCE;
	var Unix;
	var Xbox;
	var MacOSX;
}
