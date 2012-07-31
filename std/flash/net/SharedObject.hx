package flash.net;

extern class SharedObject extends flash.events.EventDispatcher {
	var client : Dynamic;
	var data(default,null) : Dynamic;
	var fps(null,default) : Float;
	var objectEncoding : UInt;
	var size(default,null) : UInt;
	function new() : Void;
	function clear() : Void;
	function close() : Void;
	function connect(myConnection : NetConnection, ?params : String) : Void;
	function flush(minDiskSpace : Int = 0) : String;
	function send(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function setDirty(propertyName : String) : Void;
	function setProperty(propertyName : String, ?value : flash.utils.Object) : Void;
	static var defaultObjectEncoding : UInt;
	static function deleteAll(url : String) : Int;
	static function getDiskUsage(url : String) : Int;
	static function getLocal(name : String, ?localPath : String, secure : Bool = false) : SharedObject;
	static function getRemote(name : String, ?remotePath : String, persistence : Dynamic = false, secure : Bool = false) : SharedObject;
}
