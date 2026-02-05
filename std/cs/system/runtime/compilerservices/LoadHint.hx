package cs.system.runtime.compilerservices;

/** Specifies the preferred default binding for a dependent assembly. */
@:native("System.Runtime.CompilerServices.LoadHint")
extern enum abstract LoadHint(Int) {
	var Always = 1;
	var Default = 0;
	var Sometimes = 2;
}
