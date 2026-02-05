package cs.system;

/** An enumeration used with the  class to specify loader optimizations for an executable. */
@:native("System.LoaderOptimization")
extern enum LoaderOptimization {
	DisallowBindings;
	DomainMask;
	MultiDomain;
	MultiDomainHost;
	NotSpecified;
	SingleDomain;
}
