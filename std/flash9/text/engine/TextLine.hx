package flash.text.engine;

@:final extern class TextLine extends flash.display.DisplayObjectContainer {
	var ascent(default,null) : Float;
	var atomCount(default,null) : Int;
	var descent(default,null) : Float;
	var hasGraphicElement(default,null) : Bool;
	var mirrorRegions(default,null) : flash.Vector<TextLineMirrorRegion>;
	var nextLine(default,null) : TextLine;
	var previousLine(default,null) : TextLine;
	var rawTextLength(default,null) : Int;
	var specifiedWidth(default,null) : Float;
	var textBlock(default,null) : TextBlock;
	var textBlockBeginIndex(default,null) : Int;
	var textHeight(default,null) : Float;
	var textWidth(default,null) : Float;
	var unjustifiedTextWidth(default,null) : Float;
	var userData : Dynamic;
	var validity : String;
	function new() : Void;
	function dump() : String;
	function flushAtomData() : Void;
	function getAtomBidiLevel(atomIndex : Int) : Int;
	function getAtomBounds(atomIndex : Int) : flash.geom.Rectangle;
	function getAtomCenter(atomIndex : Int) : Float;
	function getAtomGraphic(atomIndex : Int) : flash.display.DisplayObject;
	function getAtomIndexAtCharIndex(charIndex : Int) : Int;
	function getAtomIndexAtPoint(stageX : Float, stageY : Float) : Int;
	function getAtomTextBlockBeginIndex(atomIndex : Int) : Int;
	function getAtomTextBlockEndIndex(atomIndex : Int) : Int;
	function getAtomTextRotation(atomIndex : Int) : String;
	function getAtomWordBoundaryOnLeft(atomIndex : Int) : Bool;
	function getBaselinePosition(baseline : String) : Float;
	function getMirrorRegion(mirror : flash.events.EventDispatcher) : TextLineMirrorRegion;
	static var MAX_LINE_WIDTH : Int;
}
