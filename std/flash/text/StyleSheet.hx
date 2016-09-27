package flash.text;

extern class StyleSheet extends flash.events.EventDispatcher implements Dynamic {
	var styleNames(default,never) : Array<Dynamic>;
	function new() : Void;
	function clear() : Void;
	function getStyle(styleName : String) : flash.utils.Object;
	function parseCSS(CSSText : String) : Void;
	function setStyle(styleName : String, styleObject : flash.utils.Object) : Void;
	function transform(formatObject : flash.utils.Object) : TextFormat;
}
