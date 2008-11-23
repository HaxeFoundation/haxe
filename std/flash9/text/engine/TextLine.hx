package flash.text.engine;

extern class TextLine extends flash.display.DisplayObjectContainer {
	function new() : Void;
	var ascent(default,null) : Float;
	var atomCount(default,null) : Int;
	var descent(default,null) : Float;
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
	function getAtomTextRotation(atomIndex : Int) : flash.text.engine.TextRotation;
	function getAtomWordBoundaryOnLeft(atomIndex : Int) : Bool;
	function getBaselinePosition(baseline : flash.text.engine.TextBaseline) : Float;
	function getMirrorRegion(mirror : flash.events.EventDispatcher) : flash.text.engine.TextLineMirrorRegion;
	var hasGraphicElement(default,null) : Bool;
	var mirrorRegions(default,null) : flash.Vector<flash.text.engine.TextLineMirrorRegion>;
	var nextLine(default,null) : flash.text.engine.TextLine;
	var previousLine(default,null) : flash.text.engine.TextLine;
	var rawTextLength(default,null) : Int;
	var specifiedWidth(default,null) : Float;
	var textBlock(default,null) : flash.text.engine.TextBlock;
	var textBlockBeginIndex(default,null) : Int;
	var textHeight(default,null) : Float;
	var textWidth(default,null) : Float;
	var unjustifiedTextWidth(default,null) : Float;
	var userData : Dynamic;
	var validity : flash.text.engine.TextLineValidity;
	//private function doGetAtomIndexAtPoint(x : Float, y : Float) : Int;
	static var MAX_LINE_WIDTH : Int;
}
