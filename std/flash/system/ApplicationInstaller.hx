package flash.system;

@:final @:require(flash11_3) extern class ApplicationInstaller extends flash.events.EventDispatcher {
	var isInstalled(default,never) : Bool;
	function new() : Void;
	function install(?mode : String) : Void;
	static function iconDigest(icon : flash.utils.ByteArray) : String;
	static function stringsDigest(strings : flash.xml.XML) : String;
}
