package cs.system.runtime.interopservices;

/** Describes the type of a COM member. */
@:native("System.Runtime.InteropServices.ComMemberType")
extern enum abstract ComMemberType(Int) {
	var Method = 0;
	var PropGet = 1;
	var PropSet = 2;
}
