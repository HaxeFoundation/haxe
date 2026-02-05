package cs.system.runtime.interopservices;

/** Identifies how to expose an interface to COM. */
@:native("System.Runtime.InteropServices.ComInterfaceType")
extern enum ComInterfaceType {
	InterfaceIsDual;
	InterfaceIsIDispatch;
	InterfaceIsIInspectable;
	InterfaceIsIUnknown;
}
