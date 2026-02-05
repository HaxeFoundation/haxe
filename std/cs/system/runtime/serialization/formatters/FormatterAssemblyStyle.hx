package cs.system.runtime.serialization.formatters;

/** Indicates the method that will be used during deserialization for locating and loading assemblies. */
@:native("System.Runtime.Serialization.Formatters.FormatterAssemblyStyle")
extern enum abstract FormatterAssemblyStyle(Int) {
	var Full = 1;
	var Simple = 0;
}
