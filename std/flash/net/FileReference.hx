package flash.net;

extern class FileReference extends flash.events.EventDispatcher {
	var creationDate(default,never) : Date;
	var creator(default,never) : String;
	@:require(flash10) var data(default,never) : flash.utils.ByteArray;
	var modificationDate(default,never) : Date;
	var name(default,never) : String;
	var size(default,never) : Float;
	var type(default,never) : String;
	function new() : Void;
	function browse(?typeFilter : Array<FileFilter>) : Bool;
	function cancel() : Void;
	function download(request : URLRequest, ?defaultFileName : String) : Void;
	@:require(flash10) function load() : Void;
	@:require(flash10) function save(data : Dynamic, ?defaultFileName : String) : Void;
	function upload(request : URLRequest, ?uploadDataFieldName : String, testUpload : Bool = false) : Void;
}
