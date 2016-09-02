package flash.globalization;

@:final @:require(flash10_1) extern class LocaleID {
	var lastOperationStatus(default,never) : LastOperationStatus;
	var name(default,never) : String;
	function new(name : String) : Void;
	function getKeysAndValues() : flash.utils.Object;
	function getLanguage() : String;
	function getRegion() : String;
	function getScript() : String;
	function getVariant() : String;
	function isRightToLeft() : Bool;
	static var DEFAULT(default,never) : String;
	static function determinePreferredLocales(want : flash.Vector<String>, have : flash.Vector<String>, ?keyword : String) : flash.Vector<String>;
}
