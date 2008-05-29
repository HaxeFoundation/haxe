package flash.text.engine;

extern class TextLineMirrorRegion {
	function new() : Void;
	var bounds(default,null) : flash.geom.Rectangle;
	var element(default,null) : flash.text.engine.ContentElement;
	var mirror(default,null) : flash.events.EventDispatcher;
	var nextRegion(default,null) : flash.text.engine.TextLineMirrorRegion;
	var previousRegion(default,null) : flash.text.engine.TextLineMirrorRegion;
	var textLine(default,null) : flash.text.engine.TextLine;
}
