package flash.text.engine;

@:final extern class GraphicElement extends ContentElement {
	var elementHeight : Float;
	var elementWidth : Float;
	var graphic : flash.display.DisplayObject;
	function new(?graphic : flash.display.DisplayObject, elementWidth : Float = 15, elementHeight : Float = 15, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
}
