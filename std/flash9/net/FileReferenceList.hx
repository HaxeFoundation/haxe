package flash.net;

extern class FileReferenceList extends flash.events.EventDispatcher {
	var fileList(default,null) : Array<Dynamic>;
	function new() : Void;
	function browse(?typeFilter : Array<Dynamic>) : Bool;
}
