package flash.text.engine;

extern class ContentElement {
	function new(?elementFormat : flash.text.engine.ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : String) : Void;
	var elementFormat : flash.text.engine.ElementFormat;
	var eventMirror : flash.events.EventDispatcher;
	var groupElement(default,null) : flash.text.engine.GroupElement;
	var rawText(default,null) : String;
	var text(default,null) : String;
	var textBlock(default,null) : flash.text.engine.TextBlock;
	var textBlockBeginIndex(default,null) : Int;
	var textRotation : String;
	var userData : Dynamic;
	static var GRAPHIC_ELEMENT : UInt;
}
