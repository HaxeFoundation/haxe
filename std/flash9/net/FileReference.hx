package flash.net;

extern class FileReference extends flash.events.EventDispatcher {
	var creationDate(default,null) : Date;
	var creator(default,null) : String;
	@:require(flash10) var data(default,null) : flash.utils.ByteArray;
	var modificationDate(default,null) : Date;
	var name(default,null) : String;
	var size(default,null) : Float;
	var type(default,null) : String;
	function new() : Void;
	function browse(?typeFilter : Array<FileFilter>) : Bool;
	function cancel() : Void;
	function download(request : URLRequest, ?defaultFileName : String) : Void;
	@:require(flash10) function load() : Void;
	@:require(flash10) function save(data : Dynamic, ?defaultFileName : String) : Void;
	function upload(request : URLRequest, ?uploadDataFieldName : String, testUpload : Bool = false) : Void;
}
