package flash.system;

extern class System {
	static var ime(default,null) : IME;
	static var totalMemory(default,null) : UInt;
	static var useCodePage : Bool;
	static var vmVersion(default,null) : String;
	static function exit(code : UInt) : Void;
	static function gc() : Void;
	static function pause() : Void;
	static function resume() : Void;
	static function setClipboard(string : String) : Void;
}
