package cs.system.xml;

/** Specifies the type of validation to perform. */
@:native("System.Xml.ValidationType")
extern enum ValidationType {
	Auto;
	DTD;
	None;
	Schema;
	XDR;
}
