package cs.system.xml;

/** Represents the XML type for the string. This allows the string to be read as a particular XML type, for example a CDATA section type. */
@:native("System.Xml.XmlTokenizedType")
extern enum abstract XmlTokenizedType(Int) {
	var CDATA = 0;
	var ENTITIES = 5;
	var ENTITY = 4;
	var ENUMERATION = 9;
	var ID = 1;
	var IDREF = 2;
	var IDREFS = 3;
	var NCName = 11;
	var NMTOKEN = 6;
	var NMTOKENS = 7;
	var None = 12;
	var NOTATION = 8;
	var QName = 10;
}
