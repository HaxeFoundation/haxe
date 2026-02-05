package cs.system.data;

/** Specifies how to write XML data and a relational schema from a . */
@:native("System.Data.XmlWriteMode")
extern enum XmlWriteMode {
	DiffGram;
	IgnoreSchema;
	WriteSchema;
}
