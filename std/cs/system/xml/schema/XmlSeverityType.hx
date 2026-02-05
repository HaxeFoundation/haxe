package cs.system.xml.schema;

/** Represents the severity of the validation event. */
@:native("System.Xml.Schema.XmlSeverityType")
extern enum abstract XmlSeverityType(Int) {
	var Error = 0;
	var Warning = 1;
}
