package cs.system.data;

/** Determines the serialization format for a . */
@:native("System.Data.SerializationFormat")
extern enum abstract SerializationFormat(Int) {
	var Binary = 1;
	var Xml = 0;
}
