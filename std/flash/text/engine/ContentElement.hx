package flash.text.engine;

extern class ContentElement {
	var elementFormat : ElementFormat;
	var eventMirror : flash.events.EventDispatcher;
	var groupElement(default,null) : GroupElement;
	var rawText(default,null) : String;
	var text(default,null) : String;
	var textBlock(default,null) : TextBlock;
	var textBlockBeginIndex(default,null) : Int;
	var textRotation : TextRotation;
	var userData : Dynamic;
	function new(?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	static var GRAPHIC_ELEMENT : UInt;
}
