package cs.system.diagnostics.tracing;

/** Specifies how to format the value of a user-defined type and can be used to override the default formatting for a field. */
@:native("System.Diagnostics.Tracing.EventFieldFormat")
extern enum EventFieldFormat {
	Boolean;
	Default;
	Hexadecimal;
	HResult;
	Json;
	String;
	Xml;
}
