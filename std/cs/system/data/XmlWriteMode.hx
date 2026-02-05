package cs.system.data;

/** Specifies how to write XML data and a relational schema from a . */
@:native("System.Data.XmlWriteMode")
extern enum abstract XmlWriteMode(Int) {
	var DiffGram = 2;
	var IgnoreSchema = 1;
	var WriteSchema = 0;
}
