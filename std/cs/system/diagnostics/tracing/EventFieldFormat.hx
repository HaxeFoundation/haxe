package cs.system.diagnostics.tracing;

/** Specifies how to format the value of a user-defined type and can be used to override the default formatting for a field. */
@:native("System.Diagnostics.Tracing.EventFieldFormat")
extern enum abstract EventFieldFormat(Int) {
	var Boolean = 3;
	var Default = 0;
	var Hexadecimal = 4;
	var HResult = 15;
	var Json = 12;
	var String = 2;
	var Xml = 11;
}
