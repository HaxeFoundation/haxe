package cs.system.reflection;

/** Specifies flags for the attributes of a method implementation. */
@:native("System.Reflection.MethodImplAttributes")
extern enum abstract MethodImplAttributes(Int) {
	var AggressiveInlining = 256;
	var CodeTypeMask = 3;
	var ForwardRef = 16;
	var IL = 0;
	var InternalCall = 4096;
	var Managed = 0;
	var ManagedMask = 4;
	var MaxMethodImplVal = 65535;
	var Native = 1;
	var NoInlining = 8;
	var NoOptimization = 64;
	var OPTIL = 2;
	var PreserveSig = 128;
	var Runtime = 3;
	var Synchronized = 32;
	var Unmanaged = 4;
}
