package flash.xml;

extern class XMLDocument extends flash.xml.XMLNode {
	function new(?source : String) : Void;
	function createElement(name : String) : flash.xml.XMLNode;
	function createTextNode(text : String) : flash.xml.XMLNode;
	var docTypeDecl : Dynamic;
	var idMap : Dynamic;
	var ignoreWhite : Bool;
	function parseXML(source : String) : Void;
	var xmlDecl : Dynamic;
	private static var kElementNeverBegun : Int;
	private static var kEndOfDocument : Int;
	private static var kMalformedElement : Int;
	private static var kNoError : Int;
	private static var kOutOfMemory : Int;
	private static var kUnterminatedAttributeValue : Int;
	private static var kUnterminatedCdata : Int;
	private static var kUnterminatedComment : Int;
	private static var kUnterminatedDoctypeDeclaration : Int;
	private static var kUnterminatedElement : Int;
	private static var kUnterminatedXmlDeclaration : Int;
}
