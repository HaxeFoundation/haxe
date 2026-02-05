package cs.system.runtime.compilerservices;

/** Defines how a method is implemented. */
@:native("System.Runtime.CompilerServices.MethodCodeType")
extern enum abstract MethodCodeType(Int) {
	var IL = 0;
	var Native = 1;
	var OPTIL = 2;
	var Runtime = 3;
}
