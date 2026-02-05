package cs.system.runtime.interopservices;

/** Controls the layout of an object when exported to unmanaged code. */
@:native("System.Runtime.InteropServices.LayoutKind")
extern enum LayoutKind {
	Auto;
	Explicit;
	Sequential;
}
