package cs.system.data;

/** Indicates the schema serialization mode for a typed . */
@:native("System.Data.SchemaSerializationMode")
extern enum abstract SchemaSerializationMode(Int) {
	var ExcludeSchema = 2;
	var IncludeSchema = 1;
}
