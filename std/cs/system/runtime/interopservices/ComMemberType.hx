package cs.system.runtime.interopservices;

/** Describes the type of a COM member. */
@:native("System.Runtime.InteropServices.ComMemberType")
extern enum ComMemberType {
	Method;
	PropGet;
	PropSet;
}
