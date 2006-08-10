package flash.system;

extern class System {
	function new() : Void;
	static var ime(default,null) : flash.system.IME;
	static function setClipboard(string : String) : Void;
	static var totalMemory(default,null) : UInt;
	static var useCodePage : Bool;
	static var vmVersion(default,null) : String;
	private static var theIME : flash.system.IME;
}
