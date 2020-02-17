package flash.globalization;

@:require(flash10_1) extern final class LocaleID {
	@:flash.property var lastOperationStatus(get,never) : LastOperationStatus;
	@:flash.property var name(get,never) : String;
	function new(name : String) : Void;
	function getKeysAndValues() : flash.utils.Object;
	function getLanguage() : String;
	function getRegion() : String;
	function getScript() : String;
	function getVariant() : String;
	private function get_lastOperationStatus() : LastOperationStatus;
	private function get_name() : String;
	function isRightToLeft() : Bool;
	static final DEFAULT : String;
	static function determinePreferredLocales(want : flash.Vector<String>, have : flash.Vector<String>, ?keyword : String) : flash.Vector<String>;
}
