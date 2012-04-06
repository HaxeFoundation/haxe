package flash.system;

@:require(flash11_3) @:final extern class ApplicationInstaller {
	var isInstalled(default,null) : Bool;
	function new() : Void;
	function install(shortcutsOnly : Bool = false) : Void;
	static function iconDigest(icon : flash.utils.ByteArray) : String;
	static function stringsDigest(strings : flash.xml.XML) : String;
}
