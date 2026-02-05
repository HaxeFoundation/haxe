package cs.system.xml;

/** Specifies the type of node. */
@:native("System.Xml.XmlNodeType")
extern enum abstract XmlNodeType(Int) {
	var Attribute = 2;
	var CDATA = 4;
	var Comment = 8;
	var Document = 9;
	var DocumentFragment = 11;
	var DocumentType = 10;
	var Element = 1;
	var EndElement = 15;
	var EndEntity = 16;
	var Entity = 6;
	var EntityReference = 5;
	var None = 0;
	var Notation = 12;
	var ProcessingInstruction = 7;
	var SignificantWhitespace = 14;
	var Text = 3;
	var Whitespace = 13;
	var XmlDeclaration = 17;
}
