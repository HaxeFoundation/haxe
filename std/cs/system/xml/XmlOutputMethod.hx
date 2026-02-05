package cs.system.xml;

/** Specifies the method used to serialize the  output. */
@:native("System.Xml.XmlOutputMethod")
extern enum abstract XmlOutputMethod(Int) {
	var AutoDetect = 3;
	var Html = 1;
	var Text = 2;
	var Xml = 0;
}
