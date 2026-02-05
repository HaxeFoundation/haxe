package cs.system.data;

/** Specifies how to handle existing schema mappings when performing a  operation. */
@:native("System.Data.SchemaType")
extern enum abstract SchemaType(Int) {
	var Mapped = 2;
	var Source = 1;
}
