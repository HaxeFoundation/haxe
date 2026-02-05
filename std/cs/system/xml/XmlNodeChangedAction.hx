package cs.system.xml;

/** Specifies the type of node change. */
@:native("System.Xml.XmlNodeChangedAction")
extern enum XmlNodeChangedAction {
	Change;
	Insert;
	Remove;
}
