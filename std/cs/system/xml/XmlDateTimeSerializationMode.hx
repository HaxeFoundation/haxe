package cs.system.xml;

/** Specifies how to treat the time value when converting between string and . */
@:native("System.Xml.XmlDateTimeSerializationMode")
extern enum XmlDateTimeSerializationMode {
	Local;
	RoundtripKind;
	Unspecified;
	Utc;
}
