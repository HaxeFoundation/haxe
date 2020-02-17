package flash.text.engine;

extern final class TextLineMirrorRegion {
	@:flash.property var bounds(get,never) : flash.geom.Rectangle;
	@:flash.property var element(get,never) : ContentElement;
	@:flash.property var mirror(get,never) : flash.events.EventDispatcher;
	@:flash.property var nextRegion(get,never) : TextLineMirrorRegion;
	@:flash.property var previousRegion(get,never) : TextLineMirrorRegion;
	@:flash.property var textLine(get,never) : TextLine;
	function new() : Void;
	private function get_bounds() : flash.geom.Rectangle;
	private function get_element() : ContentElement;
	private function get_mirror() : flash.events.EventDispatcher;
	private function get_nextRegion() : TextLineMirrorRegion;
	private function get_previousRegion() : TextLineMirrorRegion;
	private function get_textLine() : TextLine;
}
