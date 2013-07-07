package flash.xml;

extern class XMLNode {
	var attributes : Dynamic;
	var childNodes(default,null) : Array<Dynamic>;
	var firstChild : XMLNode;
	var lastChild : XMLNode;
	var localName(default,null) : String;
	var namespaceURI(default,null) : String;
	var nextSibling : XMLNode;
	var nodeName : String;
	var nodeType : XMLNodeType;
	var nodeValue : String;
	var parentNode : XMLNode;
	var prefix(default,null) : String;
	var previousSibling : XMLNode;
	function new(type : XMLNodeType, value : String) : Void;
	function appendChild(node : XMLNode) : Void;
	function cloneNode(deep : Bool) : XMLNode;
	function getNamespaceForPrefix(prefix : String) : String;
	function getPrefixForNamespace(ns : String) : String;
	function hasChildNodes() : Bool;
	function insertBefore(node : XMLNode, before : XMLNode) : Void;
	function removeNode() : Void;
	function toString() : String;
}
