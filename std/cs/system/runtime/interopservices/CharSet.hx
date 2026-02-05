package cs.system.runtime.interopservices;

/** Dictates which character set marshaled strings should use. */
@:native("System.Runtime.InteropServices.CharSet")
extern enum CharSet {
	Ansi;
	Auto;
	None;
	Unicode;
}
