package cs.system;

/** An enumeration used with the  class to specify loader optimizations for an executable. */
@:native("System.LoaderOptimization")
extern enum abstract LoaderOptimization(Int) {
	var DisallowBindings = 4;
	var DomainMask = 3;
	var MultiDomain = 2;
	var MultiDomainHost = 3;
	var NotSpecified = 0;
	var SingleDomain = 1;
}
