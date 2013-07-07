package flash.xml;

extern class XMLDocument extends XMLNode {
	var docTypeDecl : Dynamic;
	var idMap : Dynamic;
	var ignoreWhite : Bool;
	var xmlDecl : Dynamic;
	function new(?source : String) : Void;
	function createElement(name : String) : XMLNode;
	function createTextNode(text : String) : XMLNode;
	function parseXML(source : String) : Void;
}
