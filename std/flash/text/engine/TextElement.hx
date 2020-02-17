package flash.text.engine;

extern final class TextElement extends ContentElement {
	function new(?text : String, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	function replaceText(beginIndex : Int, endIndex : Int, newText : String) : Void;
	private function set_text(value : String) : String;
}
