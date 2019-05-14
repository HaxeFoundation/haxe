package flash.text.engine;

extern final class FontDescription {
	@:flash.property var cffHinting(get,set) : CFFHinting;
	@:flash.property var fontLookup(get,set) : FontLookup;
	@:flash.property var fontName(get,set) : String;
	@:flash.property var fontPosture(get,set) : FontPosture;
	@:flash.property var fontWeight(get,set) : FontWeight;
	@:flash.property var locked(get,set) : Bool;
	@:flash.property var renderingMode(get,set) : RenderingMode;
	function new(?fontName : String, ?fontWeight : FontWeight, ?fontPosture : FontPosture, ?fontLookup : FontLookup, ?renderingMode : RenderingMode, ?cffHinting : CFFHinting) : Void;
	function clone() : FontDescription;
	private function get_cffHinting() : CFFHinting;
	private function get_fontLookup() : FontLookup;
	private function get_fontName() : String;
	private function get_fontPosture() : FontPosture;
	private function get_fontWeight() : FontWeight;
	private function get_locked() : Bool;
	private function get_renderingMode() : RenderingMode;
	private function set_cffHinting(value : CFFHinting) : CFFHinting;
	private function set_fontLookup(value : FontLookup) : FontLookup;
	private function set_fontName(value : String) : String;
	private function set_fontPosture(value : FontPosture) : FontPosture;
	private function set_fontWeight(value : FontWeight) : FontWeight;
	private function set_locked(value : Bool) : Bool;
	private function set_renderingMode(value : RenderingMode) : RenderingMode;
	@:require(flash10_1) static function isDeviceFontCompatible(fontName : String, fontWeight : FontWeight, fontPosture : FontPosture) : Bool;
	static function isFontCompatible(fontName : String, fontWeight : FontWeight, fontPosture : FontPosture) : Bool;
}
