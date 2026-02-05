package cs.system.runtime.interopservices;

/** Provides return values for the  method. */
@:native("System.Runtime.InteropServices.CustomQueryInterfaceResult")
extern enum abstract CustomQueryInterfaceResult(Int) {
	var Failed = 2;
	var Handled = 0;
	var NotHandled = 1;
}
