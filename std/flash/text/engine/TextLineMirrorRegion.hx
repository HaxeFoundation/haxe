package flash.text.engine;

@:final extern class TextLineMirrorRegion {
	var bounds(default,null) : flash.geom.Rectangle;
	var element(default,null) : ContentElement;
	var mirror(default,null) : flash.events.EventDispatcher;
	var nextRegion(default,null) : TextLineMirrorRegion;
	var previousRegion(default,null) : TextLineMirrorRegion;
	var textLine(default,null) : TextLine;
	function new() : Void;
}
