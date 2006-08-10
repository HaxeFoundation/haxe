package flash.net;

extern class FileReferenceList extends flash.events.EventDispatcher {
	function new() : Void;
	function browse(?typeFilter : Array<Dynamic>) : Bool;
	var fileList(default,null) : Array<Dynamic>;
}
