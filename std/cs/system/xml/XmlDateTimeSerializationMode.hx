package cs.system.xml;

/** Specifies how to treat the time value when converting between string and . */
@:native("System.Xml.XmlDateTimeSerializationMode")
extern enum abstract XmlDateTimeSerializationMode(Int) {
	var Local = 0;
	var RoundtripKind = 3;
	var Unspecified = 2;
	var Utc = 1;
}
