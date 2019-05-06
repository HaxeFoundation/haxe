package flash.text.engine;

extern final class GraphicElement extends ContentElement {
	@:flash.property var elementHeight(get,set) : Float;
	@:flash.property var elementWidth(get,set) : Float;
	@:flash.property var graphic(get,set) : flash.display.DisplayObject;
	function new(?graphic : flash.display.DisplayObject, elementWidth : Float = 15, elementHeight : Float = 15, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	private function get_elementHeight() : Float;
	private function get_elementWidth() : Float;
	private function get_graphic() : flash.display.DisplayObject;
	private function set_elementHeight(value : Float) : Float;
	private function set_elementWidth(value : Float) : Float;
	private function set_graphic(value : flash.display.DisplayObject) : flash.display.DisplayObject;
}
