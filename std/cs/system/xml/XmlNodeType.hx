package cs.system.xml;

/** Specifies the type of node. */
@:native("System.Xml.XmlNodeType")
extern enum XmlNodeType {
	Attribute;
	CDATA;
	Comment;
	Document;
	DocumentFragment;
	DocumentType;
	Element;
	EndElement;
	EndEntity;
	Entity;
	EntityReference;
	None;
	Notation;
	ProcessingInstruction;
	SignificantWhitespace;
	Text;
	Whitespace;
	XmlDeclaration;
}
