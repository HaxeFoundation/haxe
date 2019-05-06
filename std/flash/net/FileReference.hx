package flash.net;

extern class FileReference extends flash.events.EventDispatcher {
	@:flash.property var creationDate(get,never) : Date;
	@:flash.property var creator(get,never) : String;
	@:flash.property @:require(flash10) var data(get,never) : flash.utils.ByteArray;
	@:flash.property var modificationDate(get,never) : Date;
	@:flash.property var name(get,never) : String;
	@:flash.property var size(get,never) : Float;
	@:flash.property var type(get,never) : String;
	function new() : Void;
	function browse(?typeFilter : Array<FileFilter>) : Bool;
	function cancel() : Void;
	function download(request : URLRequest, ?defaultFileName : String) : Void;
	private function get_creationDate() : Date;
	private function get_creator() : String;
	private function get_data() : flash.utils.ByteArray;
	private function get_modificationDate() : Date;
	private function get_name() : String;
	private function get_size() : Float;
	private function get_type() : String;
	@:require(flash10) function load() : Void;
	@:require(flash10) function save(data : Dynamic, ?defaultFileName : String) : Void;
	function upload(request : URLRequest, ?uploadDataFieldName : String, testUpload : Bool = false) : Void;
}
