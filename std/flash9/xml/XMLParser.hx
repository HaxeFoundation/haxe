package flash.xml;

extern class XMLParser {
	function new() : Void;
	function getNext(tag : flash.xml.XMLTag) : Int;
	function startParse(source : String, ignoreWhite : Bool) : Void;
}
