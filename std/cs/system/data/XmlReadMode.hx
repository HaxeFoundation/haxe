package cs.system.data;

/** Specifies how to read XML data and a relational schema into a . */
@:native("System.Data.XmlReadMode")
extern enum XmlReadMode {
	Auto;
	DiffGram;
	Fragment;
	IgnoreSchema;
	InferSchema;
	InferTypedSchema;
	ReadSchema;
}
