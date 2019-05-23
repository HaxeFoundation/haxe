package flash.xml;

extern class XMLNode {
	@:flash.property var attributes(get,set) : Dynamic;
	@:flash.property var childNodes(get,never) : Array<Dynamic>;
	var firstChild : XMLNode;
	var lastChild : XMLNode;
	@:flash.property var localName(get,never) : String;
	@:flash.property var namespaceURI(get,never) : String;
	var nextSibling : XMLNode;
	var nodeName : String;
	var nodeType : XMLNodeType;
	var nodeValue : String;
	var parentNode : XMLNode;
	@:flash.property var prefix(get,never) : String;
	var previousSibling : XMLNode;
	function new(type : XMLNodeType, value : String) : Void;
	function appendChild(node : XMLNode) : Void;
	function cloneNode(deep : Bool) : XMLNode;
	function getNamespaceForPrefix(prefix : String) : String;
	function getPrefixForNamespace(ns : String) : String;
	private function get_attributes() : Dynamic;
	private function get_childNodes() : Array<Dynamic>;
	private function get_localName() : String;
	private function get_namespaceURI() : String;
	private function get_prefix() : String;
	function hasChildNodes() : Bool;
	function insertBefore(node : XMLNode, before : XMLNode) : Void;
	function removeNode() : Void;
	private function set_attributes(value : Dynamic) : Dynamic;
	function toString() : String;
}
