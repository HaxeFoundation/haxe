package flash.system;

extern class System {
	@:require(flash10_1) static var currentTime(default,null) : Float;
	@:require(flash10_1) static var freeMemory(default,null) : Float;
	static var ime(default,null) : IME;
	@:require(flash10_1) static var preciseStartupTime(default,null) : Float;
	@:require(flash10_1) static var privateMemory(default,null) : Float;
	static var totalMemory(default,null) : UInt;
	@:require(flash10_1) static var totalMemoryNumber(default,null) : Float;
	static var useCodePage : Bool;
	static var vmVersion(default,null) : String;
	@:require(flash10_1) static function disposeXML(node : flash.xml.XML) : Void;
	static function exit(code : UInt) : Void;
	static function gc() : Void;
	@:require(flash10_1) static function nativeConstructionOnly(object : Dynamic) : Void;
	static function pause() : Void;
	static function resume() : Void;
	static function setClipboard(string : String) : Void;
}
