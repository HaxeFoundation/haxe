package cs.system.runtime.interopservices;

/** Provides return values for the  method. */
@:native("System.Runtime.InteropServices.CustomQueryInterfaceResult")
extern enum CustomQueryInterfaceResult {
	Failed;
	Handled;
	NotHandled;
}
