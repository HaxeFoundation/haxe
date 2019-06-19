package flash.text.engine;

extern class ContentElement {
	@:flash.property var elementFormat(get,set) : ElementFormat;
	@:flash.property var eventMirror(get,set) : flash.events.EventDispatcher;
	@:flash.property var groupElement(get,never) : GroupElement;
	@:flash.property var rawText(get,never) : String;
	@:flash.property var text(get,never) : String;
	@:flash.property var textBlock(get,never) : TextBlock;
	@:flash.property var textBlockBeginIndex(get,never) : Int;
	@:flash.property var textRotation(get,set) : TextRotation;
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
