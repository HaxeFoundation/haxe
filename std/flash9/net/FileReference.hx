package flash.net;

extern class FileReference extends flash.events.EventDispatcher {
	function new() : Void;
	function browse(?typeFilter : Array<Dynamic>) : Bool;
	function cancel() : Void;
	var creationDate(default,null) : Date;
	var creator(default,null) : String;
	function download(request : flash.net.URLRequest, ?defaultFileName : String) : Void;
	var modificationDate(default,null) : Date;
	var name(default,null) : String;
	var size(default,null) : UInt;
	var type(default,null) : String;
	function upload(request : flash.net.URLRequest, ?uploadDataFieldName : String, ?testUpload : Bool) : Void;
}
