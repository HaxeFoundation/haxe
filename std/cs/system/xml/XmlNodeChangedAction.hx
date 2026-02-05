package cs.system.xml;

/** Specifies the type of node change. */
@:native("System.Xml.XmlNodeChangedAction")
extern enum abstract XmlNodeChangedAction(Int) {
	var Change = 2;
	var Insert = 0;
	var Remove = 1;
}
