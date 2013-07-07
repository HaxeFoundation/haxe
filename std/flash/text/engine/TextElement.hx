package flash.text.engine;

@:final extern class TextElement extends ContentElement {
	function new(?text : String, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	function replaceText(beginIndex : Int, endIndex : Int, newText : String) : Void;
}
