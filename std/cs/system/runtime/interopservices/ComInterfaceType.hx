package cs.system.runtime.interopservices;

/** Identifies how to expose an interface to COM. */
@:native("System.Runtime.InteropServices.ComInterfaceType")
extern enum abstract ComInterfaceType(Int) {
	var InterfaceIsDual = 0;
	var InterfaceIsIDispatch = 2;
	var InterfaceIsIInspectable = 3;
	var InterfaceIsIUnknown = 1;
}
