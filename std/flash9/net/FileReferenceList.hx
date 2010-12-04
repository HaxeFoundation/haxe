package flash.net;

extern class FileReferenceList extends flash.events.EventDispatcher {
	var fileList(default,null) : Array<FileReference>;
	function new() : Void;
	function browse(?typeFilter : Array<FileFilter>) : Bool;
}
