package cs.system.runtime.interopservices;

/** Indicates whether the  method's IUnknown::QueryInterface calls can use the  interface. */
@:native("System.Runtime.InteropServices.CustomQueryInterfaceMode")
extern enum CustomQueryInterfaceMode {
	Allow;
	Ignore;
}
