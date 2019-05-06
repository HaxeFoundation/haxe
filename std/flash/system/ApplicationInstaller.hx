package flash.system;

@:require(flash11_3) extern final class ApplicationInstaller extends flash.events.EventDispatcher {
	@:flash.property var isInstalled(get,never) : Bool;
	function new() : Void;
	private function get_isInstalled() : Bool;
	function install(?mode : String) : Void;
	static function iconDigest(icon : flash.utils.ByteArray) : String;
	static function stringsDigest(strings : flash.xml.XML) : String;
}
