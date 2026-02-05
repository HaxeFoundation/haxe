package cs.system.runtime.serialization.formatters;

/** Indicates the method that will be used during deserialization for locating and loading assemblies. */
@:native("System.Runtime.Serialization.Formatters.FormatterAssemblyStyle")
extern enum FormatterAssemblyStyle {
	Full;
	Simple;
}
