package cs.system.runtime.compilerservices;

/** Defines how a method is implemented. */
@:native("System.Runtime.CompilerServices.MethodCodeType")
extern enum MethodCodeType {
	IL;
	Native;
	OPTIL;
	Runtime;
}
