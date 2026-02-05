package cs.system.reflection;

/** Specifies flags for the attributes of a method implementation. */
@:native("System.Reflection.MethodImplAttributes")
extern enum MethodImplAttributes {
	AggressiveInlining;
	CodeTypeMask;
	ForwardRef;
	IL;
	InternalCall;
	Managed;
	ManagedMask;
	MaxMethodImplVal;
	Native;
	NoInlining;
	NoOptimization;
	OPTIL;
	PreserveSig;
	Runtime;
	Synchronized;
	Unmanaged;
}
