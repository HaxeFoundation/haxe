package flash.xml;

extern class XMLNode {
	function new(type : UInt, value : String) : Void;
	function appendChild(node : flash.xml.XMLNode) : Void;
	var attributes : Dynamic;
	var childNodes(default,null) : Array<Dynamic>;
	function cloneNode(deep : Bool) : flash.xml.XMLNode;
	var firstChild : flash.xml.XMLNode;
	function getNamespaceForPrefix(prefix : String) : String;
	function getPrefixForNamespace(ns : String) : String;
	function hasChildNodes() : Bool;
	function insertBefore(node : flash.xml.XMLNode, before : flash.xml.XMLNode) : Void;
	var lastChild : flash.xml.XMLNode;
	var localName(default,null) : String;
	var namespaceURI(default,null) : String;
	var nextSibling : flash.xml.XMLNode;
	var nodeName : String;
	var nodeType : UInt;
	var nodeValue : String;
	var parentNode : flash.xml.XMLNode;
	var prefix(default,null) : String;
	var previousSibling : flash.xml.XMLNode;
	function removeNode() : Void;
	function toString() : String;
	private var _attributes : Dynamic;
	private var _childNodes : Array<Dynamic>;
	private function escapeXML(value : String) : String;
	private function init(type : UInt, value : String) : Void;
}
