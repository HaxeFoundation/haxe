package cs.system.runtime.interopservices;

/** Represents the types of handles the  class can allocate. */
@:native("System.Runtime.InteropServices.GCHandleType")
extern enum GCHandleType {
	Normal;
	Pinned;
	Weak;
	WeakTrackResurrection;
}
