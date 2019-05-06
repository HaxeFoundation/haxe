package flash.system;

extern class System {
	@:flash.property @:require(flash10_1) static var freeMemory(get,never) : Float;
	@:flash.property static var ime(get,never) : IME;
	@:flash.property @:require(flash10_1) static var privateMemory(get,never) : Float;
	@:flash.property @:require(flash11) static var processCPUUsage(get,never) : Float;
	@:flash.property static var totalMemory(get,never) : UInt;
	@:flash.property @:require(flash10_1) static var totalMemoryNumber(get,never) : Float;
	@:flash.property static var useCodePage(get,set) : Bool;
	@:flash.property static var vmVersion(get,never) : String;
	@:require(flash10_1) static function disposeXML(node : flash.xml.XML) : Void;
	static function exit(code : UInt) : Void;
	static function gc() : Void;
	private static function get_freeMemory() : Float;
	private static function get_ime() : IME;
	private static function get_privateMemory() : Float;
	private static function get_processCPUUsage() : Float;
	private static function get_totalMemory() : UInt;
	private static function get_totalMemoryNumber() : Float;
	private static function get_useCodePage() : Bool;
	private static function get_vmVersion() : String;
	static function pause() : Void;
	@:require(flash11) static function pauseForGCIfCollectionImminent(imminence : Float = 0.75) : Void;
	static function resume() : Void;
	static function setClipboard(string : String) : Void;
	private static function set_useCodePage(value : Bool) : Bool;
}
