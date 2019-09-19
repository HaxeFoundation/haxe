package haxe;

extern class Error {
	public var message(get, never):String;
	public final posInfos:haxe.PosInfos;
	public final type:Int;

	function get_message():String;
}
