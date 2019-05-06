package flash.net;

extern class FileReferenceList extends flash.events.EventDispatcher {
	@:flash.property var fileList(get,never) : Array<FileReference>;
	function new() : Void;
	function browse(?typeFilter : Array<FileFilter>) : Bool;
	private function get_fileList() : Array<FileReference>;
}
