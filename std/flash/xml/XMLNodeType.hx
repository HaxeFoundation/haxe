package flash.xml;

@:native("flash.xml.XMLNodeType") extern enum abstract XMLNodeType(UInt) {
	var CDATA_NODE;
	var COMMENT_NODE;
	var DOCUMENT_TYPE_NODE;
	var ELEMENT_NODE;
	var PROCESSING_INSTRUCTION_NODE;
	var TEXT_NODE;
	var XML_DECLARATION;
}
