package cs.system.runtime.serialization.formatters;

/** Indicates the format in which type descriptions are laid out in the serialized stream. */
@:native("System.Runtime.Serialization.Formatters.FormatterTypeStyle")
extern enum abstract FormatterTypeStyle(Int) {
	var TypesAlways = 1;
	var TypesWhenNeeded = 0;
	var XsdString = 2;
}
