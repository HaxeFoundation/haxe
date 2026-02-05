package cs.system.xml;

/** Specifies the options for processing DTDs. The  enumeration is used by the  class. */
@:native("System.Xml.DtdProcessing")
extern enum abstract DtdProcessing(Int) {
	var Ignore = 1;
	var Parse = 2;
	var Prohibit = 0;
}
