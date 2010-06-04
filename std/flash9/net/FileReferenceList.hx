package flash.net;

extern class FileReferenceList extends flash.events.EventDispatcher {
	var fileList(default,null) : Array<flash.net.FileReference>;
	function new() : Void;
	function browse(?typeFilter : Array<flash.net.FileFilter>) : Bool;
}
