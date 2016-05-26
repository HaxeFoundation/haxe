package flash.system;

extern class System {
	@:require(flash10_1) static var freeMemory(default,never) : Float;
	static var ime(default,never) : IME;
	@:require(flash10_1) static var privateMemory(default,never) : Float;
	@:require(flash11) static var processCPUUsage(default,never) : Float;
	static var totalMemory(default,never) : UInt;
	@:require(flash10_1) static var totalMemoryNumber(default,never) : Float;
	static var useCodePage : Bool;
	static var vmVersion(default,never) : String;
	@:require(flash10_1) static function disposeXML(node : flash.xml.XML) : Void;
	static function exit(code : UInt) : Void;
	static function gc() : Void;
	static function pause() : Void;
	@:require(flash11) static function pauseForGCIfCollectionImminent(imminence : Float = 0.75) : Void;
	static function resume() : Void;
	static function setClipboard(string : String) : Void;
}
