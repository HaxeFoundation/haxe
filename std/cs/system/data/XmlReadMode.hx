package cs.system.data;

/** Specifies how to read XML data and a relational schema into a . */
@:native("System.Data.XmlReadMode")
extern enum abstract XmlReadMode(Int) {
	var Auto = 0;
	var DiffGram = 4;
	var Fragment = 5;
	var IgnoreSchema = 2;
	var InferSchema = 3;
	var InferTypedSchema = 6;
	var ReadSchema = 1;
}
