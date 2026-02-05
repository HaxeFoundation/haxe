package cs.system.xml;

/** Specifies the current  scope. */
@:native("System.Xml.XmlSpace")
extern enum abstract XmlSpace(Int) {
	var Default = 1;
	var None = 0;
	var Preserve = 2;
}
