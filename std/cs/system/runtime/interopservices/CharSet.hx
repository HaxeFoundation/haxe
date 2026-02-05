package cs.system.runtime.interopservices;

/** Dictates which character set marshaled strings should use. */
@:native("System.Runtime.InteropServices.CharSet")
extern enum abstract CharSet(Int) {
	var Ansi = 2;
	var Auto = 4;
	var None = 1;
	var Unicode = 3;
}
