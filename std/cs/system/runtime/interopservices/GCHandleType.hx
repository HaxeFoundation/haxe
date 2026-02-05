package cs.system.runtime.interopservices;

/** Represents the types of handles the  class can allocate. */
@:native("System.Runtime.InteropServices.GCHandleType")
extern enum abstract GCHandleType(Int) {
	var Normal = 2;
	var Pinned = 3;
	var Weak = 0;
	var WeakTrackResurrection = 1;
}
