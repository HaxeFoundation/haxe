package cs.system.runtime.compilerservices;

/** Specifies the preferred default binding for a dependent assembly. */
@:native("System.Runtime.CompilerServices.LoadHint")
extern enum LoadHint {
	Always;
	Default;
	Sometimes;
}
