package flash.text.engine;

extern class GraphicElement extends flash.text.engine.ContentElement {
	function new(?graphic : flash.display.DisplayObject, ?elementWidth : Float, ?elementHeight : Float, ?elementFormat : flash.text.engine.ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : flash.text.engine.TextRotation) : Void;
	var elementHeight : Float;
	var elementWidth : Float;
	var graphic : flash.display.DisplayObject;
}
