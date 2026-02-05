package cs.system.xml;

/** Represents the XML type for the string. This allows the string to be read as a particular XML type, for example a CDATA section type. */
@:native("System.Xml.XmlTokenizedType")
extern enum XmlTokenizedType {
	CDATA;
	ENTITIES;
	ENTITY;
	ENUMERATION;
	ID;
	IDREF;
	IDREFS;
	NCName;
	NMTOKEN;
	NMTOKENS;
	None;
	NOTATION;
	QName;
}
