package flash.text.engine;

extern final class TextLineMirrorRegion {
	var bounds(get,never) : flash.geom.Rectangle;
	var element(get,never) : ContentElement;
	var mirror(get,never) : flash.events.EventDispatcher;
	var nextRegion(get,never) : TextLineMirrorRegion;
	var previousRegion(get,never) : TextLineMirrorRegion;
	var textLine(get,never) : TextLine;
	function new() : Void;
	private function get_bounds() : flash.geom.Rectangle;
	private function get_element() : ContentElement;
	private function get_mirror() : flash.events.EventDispatcher;
	private function get_nextRegion() : TextLineMirrorRegion;
	private function get_previousRegion() : TextLineMirrorRegion;
	private function get_textLine() : TextLine;
}
