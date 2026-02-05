package cs.system.runtime.interopservices;

/** Identifies the type of class interface that is generated for a class. */
@:native("System.Runtime.InteropServices.ClassInterfaceType")
extern enum ClassInterfaceType {
	AutoDispatch;
	AutoDual;
	None;
}
