package flash.text.engine;

extern class ContentElement {
	var elementFormat(get,set) : ElementFormat;
	var eventMirror(get,set) : flash.events.EventDispatcher;
	var groupElement(get,never) : GroupElement;
	var rawText(get,never) : String;
	var text(get,never) : String;
	var textBlock(get,never) : TextBlock;
	var textBlockBeginIndex(get,never) : Int;
	var textRotation(get,set) : TextRotation;
	var userData : Dynamic;
	function new(?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	private function get_elementFormat() : ElementFormat;
	private function get_eventMirror() : flash.events.EventDispatcher;
	private function get_groupElement() : GroupElement;
	private function get_rawText() : String;
	private function get_text() : String;
	private function get_textBlock() : TextBlock;
	private function get_textBlockBeginIndex() : Int;
	private function get_textRotation() : TextRotation;
	private function set_elementFormat(value : ElementFormat) : ElementFormat;
	private function set_eventMirror(value : flash.events.EventDispatcher) : flash.events.EventDispatcher;
	private function set_textRotation(value : TextRotation) : TextRotation;
	static final GRAPHIC_ELEMENT : UInt;
}
