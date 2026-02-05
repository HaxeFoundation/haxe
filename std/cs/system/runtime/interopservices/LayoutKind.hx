package cs.system.runtime.interopservices;

/** Controls the layout of an object when exported to unmanaged code. */
@:native("System.Runtime.InteropServices.LayoutKind")
extern enum abstract LayoutKind(Int) {
	var Auto = 3;
	var Explicit = 2;
	var Sequential = 0;
}
