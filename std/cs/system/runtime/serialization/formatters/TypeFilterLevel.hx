package cs.system.runtime.serialization.formatters;

/** Specifies the level of automatic deserialization for .NET Framework remoting. */
@:native("System.Runtime.Serialization.Formatters.TypeFilterLevel")
extern enum abstract TypeFilterLevel(Int) {
	var Full = 3;
	var Low = 2;
}
