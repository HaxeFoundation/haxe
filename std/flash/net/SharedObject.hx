package flash.net;

extern class SharedObject extends flash.events.EventDispatcher {
	var client(get,set) : Dynamic;
	var data(get,never) : Dynamic;
	var fps(never,set) : Float;
	var objectEncoding(get,set) : UInt;
	var size(get,never) : UInt;
	function new() : Void;
	function clear() : Void;
	function close() : Void;
	function connect(myConnection : NetConnection, ?params : String) : Void;
	function flush(minDiskSpace : Int = 0) : String;
	private function get_client() : Dynamic;
	private function get_data() : Dynamic;
	private function get_objectEncoding() : UInt;
	private function get_size() : UInt;
	function send(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function setDirty(propertyName : String) : Void;
	function setProperty(propertyName : String, ?value : flash.utils.Object) : Void;
	private function set_client(value : Dynamic) : Dynamic;
	private function set_fps(value : Float) : Float;
	private function set_objectEncoding(value : UInt) : UInt;
	static var defaultObjectEncoding(get,set) : UInt;
	@:require(flash11_7) static var preventBackup(get,set) : Bool;
	static function deleteAll(url : String) : Int;
	static function getDiskUsage(url : String) : Int;
	static function getLocal(name : String, ?localPath : String, secure : Bool = false) : SharedObject;
	static function getRemote(name : String, ?remotePath : String, persistence : Dynamic = false, secure : Bool = false) : SharedObject;
	private static function get_defaultObjectEncoding() : UInt;
	private static function get_preventBackup() : Bool;
	private static function set_defaultObjectEncoding(value : UInt) : UInt;
	private static function set_preventBackup(value : Bool) : Bool;
}
