package flash.text.engine;

@:final extern class TextBlock {
	var applyNonLinearFontScaling : Bool;
	var baselineFontDescription : FontDescription;
	var baselineFontSize : Float;
	var baselineZero : TextBaseline;
	var bidiLevel : Int;
	var content : ContentElement;
	var firstInvalidLine(default,never) : TextLine;
	var firstLine(default,never) : TextLine;
	var lastLine(default,never) : TextLine;
	var lineRotation : TextRotation;
	var tabStops : flash.Vector<TabStop>;
	var textJustifier : TextJustifier;
	var textLineCreationResult(default,never) : TextLineCreationResult;
	var userData : Dynamic;
	function new(?content : ContentElement, ?tabStops : flash.Vector<TabStop>, ?textJustifier : TextJustifier, ?lineRotation : TextRotation, ?baselineZero : TextBaseline, bidiLevel : Int = 0, applyNonLinearFontScaling : Bool = true, ?baselineFontDescription : FontDescription, baselineFontSize : Float = 12) : Void;
	function createTextLine(?previousLine : TextLine, width : Float = 1000000, lineOffset : Float = 0, fitSomething : Bool = false) : TextLine;
	function dump() : String;
	function findNextAtomBoundary(afterCharIndex : Int) : Int;
	function findNextWordBoundary(afterCharIndex : Int) : Int;
	function findPreviousAtomBoundary(beforeCharIndex : Int) : Int;
	function findPreviousWordBoundary(beforeCharIndex : Int) : Int;
	function getTextLineAtCharIndex(charIndex : Int) : TextLine;
	@:require(flash10_1) function recreateTextLine(textLine : TextLine, ?previousLine : TextLine, width : Float = 1000000, lineOffset : Float = 0, fitSomething : Bool = false) : TextLine;
	@:require(flash10_1) function releaseLineCreationData() : Void;
	function releaseLines(firstLine : TextLine, lastLine : TextLine) : Void;
}
