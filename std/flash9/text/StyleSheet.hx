package flash.text;

extern class StyleSheet extends flash.events.EventDispatcher {
	function new() : Void;
	function clear() : Void;
	function getStyle(styleName : String) : Dynamic;
	function parseCSS(CSSText : String) : Void;
	function setStyle(styleName : String, styleObject : Dynamic) : Void;
	var styleNames(default,null) : Array<Dynamic>;
	function transform(formatObject : Dynamic) : flash.text.TextFormat;
	private function _copy(o : Dynamic) : Dynamic;
	private var _css : Dynamic;
	private function _parseCSSFontFamily(fontFamily : String) : String;
	private function _parseCSSInternal(cssText : String) : Dynamic;
	private function _parseColor(color : String) : UInt;
	private var _styles : Dynamic;
	private function _update() : Void;
	private function doTransform(n : String) : Void;
}
