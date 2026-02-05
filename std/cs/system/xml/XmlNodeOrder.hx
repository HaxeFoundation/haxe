package cs.system.xml;

/** Describes the document order of a node compared to a second node. */
@:native("System.Xml.XmlNodeOrder")
extern enum XmlNodeOrder {
	After;
	Before;
	Same;
	Unknown;
}
