package cs.system.xml;

/** Describes the document order of a node compared to a second node. */
@:native("System.Xml.XmlNodeOrder")
extern enum abstract XmlNodeOrder(Int) {
	var After = 1;
	var Before = 0;
	var Same = 2;
	var Unknown = 3;
}
