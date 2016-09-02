package flash.text.engine;

extern class ContentElement {
	var elementFormat : ElementFormat;
	var eventMirror : flash.events.EventDispatcher;
	var groupElement(default,never) : GroupElement;
	var rawText(default,never) : String;
	var text(default,never) : String;
	var textBlock(default,never) : TextBlock;
	var textBlockBeginIndex(default,never) : Int;
	var textRotation : TextRotation;
	var userData : Dynamic;
	function new(?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	static var GRAPHIC_ELEMENT(default,never) : UInt;
}
