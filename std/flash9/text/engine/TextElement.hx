package flash.text.engine;

extern class TextElement extends flash.text.engine.ContentElement {
	function new(?text : String, ?elementFormat : flash.text.engine.ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : String) : Void;
	function replaceText(beginIndex : Int, endIndex : Int, newText : String) : Void;
	//var text(null,default) : Void;
}
