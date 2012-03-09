package flash.xml;

@:final extern class XMLParser {
	function new() : Void;
	function getNext(tag : XMLTag) : Int;
	function startParse(source : String, ignoreWhite : Bool) : Void;
}
