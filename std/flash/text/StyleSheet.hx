package flash.text;

extern class StyleSheet extends flash.events.EventDispatcher, implements Dynamic {
	var styleNames(default,null) : Array<Dynamic>;
	function new() : Void;
	function clear() : Void;
	function getStyle(styleName : String) : Dynamic;
	function parseCSS(CSSText : String) : Void;
	function setStyle(styleName : String, styleObject : Dynamic) : Void;
	function transform(formatObject : Dynamic) : TextFormat;
}
