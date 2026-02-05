package cs.system.xml;

/** Specifies how to handle line breaks. */
@:native("System.Xml.NewLineHandling")
extern enum abstract NewLineHandling(Int) {
	var Entitize = 1;
	var None = 2;
	var Replace = 0;
}
