package flash.system;

extern class System {
	function new() : Void;
	static var ime(default,null) : flash.system.IME;
	static function setClipboard(string : String) : Void;
	static var totalMemory(default,null) : UInt;
	static var useCodePage : Bool;
	static var vmVersion(default,null) : String;
	private static var theIME : flash.system.IME;

	/** for FP 9.0.115+ debug only */
	static function exit(code : UInt) : Void;

	/** for FP 9.0.115+ debug only */
	static function gc() : Void;

	/** for FP 9.0.115+ debug only */
	static function pause() : Void;

	/** for FP 9.0.115+ debug only */
	static function resume() : Void;
	
}
