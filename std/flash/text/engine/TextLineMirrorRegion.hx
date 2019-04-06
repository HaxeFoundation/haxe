package flash.text.engine;

extern final class TextLineMirrorRegion {
	var bounds(default,never) : flash.geom.Rectangle;
	var element(default,never) : ContentElement;
	var mirror(default,never) : flash.events.EventDispatcher;
	var nextRegion(default,never) : TextLineMirrorRegion;
	var previousRegion(default,never) : TextLineMirrorRegion;
	var textLine(default,never) : TextLine;
	function new() : Void;
}
