package flash.net;

extern class SharedObject extends flash.events.EventDispatcher {
	function new() : Void;
	function clear() : Void;
	var client : Dynamic;
	function close() : Void;
	function connect(myConnection : flash.net.NetConnection, ?params : String) : Void;
	var data(default,null) : Dynamic;
	function flush(?minDiskSpace : Int) : String;
	var fps(null,default) : Void;
	var objectEncoding : UInt;
	function send( /* ...arguments */) : Void;
	function setDirty(propertyName : String) : Void;
	function setProperty(propertyName : String, ?value : Dynamic) : Void;
	var size(default,null) : UInt;
	private function invoke(index : UInt /* ...arguments */) : Void;
	private function invokeWithArgsArray(index : UInt, args : Array<Dynamic>) : Void;
	static var defaultObjectEncoding : UInt;
	static function deleteAll(url : String) : Int;
	static function getDiskUsage(url : String) : Int;
	static function getLocal(name : String, ?localPath : String, ?secure : Bool) : flash.net.SharedObject;
	static function getRemote(name : String, ?remotePath : String, ?persistence : Dynamic, ?secure : Bool) : flash.net.SharedObject;
	private static var kClear : UInt;
	private static var kClose : UInt;
	private static var kConnect : UInt;
	private static var kFlush : UInt;
	private static var kGetSize : UInt;
	private static var kSend : UInt;
	private static var kSetFps : UInt;
}
