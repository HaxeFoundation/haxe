package flash.text.engine;

extern class TextBlock {
	function new(?content : flash.text.engine.ContentElement, ?tabStops : flash.Vector<flash.text.engine.TabStop>, ?textJustifier : flash.text.engine.TextJustifier, ?glyphRotation : String, ?lineRotation : String, ?baselineZero : String, ?bidiLevel : Int, ?applyNonLinearFontScaling : Bool, ?baselineFontDescription : flash.text.engine.FontDescription, ?baselineFontSize : Float) : Void;
	var applyNonLinearFontScaling : Bool;
	var baselineFontDescription : flash.text.engine.FontDescription;
	var baselineFontSize : Float;
	var baselineZero : String;
	var bidiLevel : Int;
	var content : flash.text.engine.ContentElement;
	function createTextLine(?previousLine : flash.text.engine.TextLine, ?width : Float, ?lineOffset : Float, ?fitSomething : Bool) : flash.text.engine.TextLine;
	function dump() : String;
	function findNextAtomBoundary(afterCharIndex : Int) : Int;
	function findNextWordBoundary(afterCharIndex : Int) : Int;
	function findPreviousAtomBoundary(beforeCharIndex : Int) : Int;
	function findPreviousWordBoundary(beforeCharIndex : Int) : Int;
	var firstInvalidLine(default,null) : flash.text.engine.TextLine;
	var firstLine(default,null) : flash.text.engine.TextLine;
	function getTextLineAtCharIndex(charIndex : Int) : flash.text.engine.TextLine;
	var glyphRotation : String;
	var lastLine(default,null) : flash.text.engine.TextLine;
	var lineRotation : String;
	function releaseLines(firstLine : flash.text.engine.TextLine, lastLine : flash.text.engine.TextLine) : Dynamic;
	var tabStops : flash.Vector<flash.text.engine.TabStop>;
	var textJustifier : flash.text.engine.TextJustifier;
	var textLineCreationResult(default,null) : String;
	var userData : Dynamic;
	//private function DoCreateTextLine(previousLine : flash.text.engine.TextLine, width : Float, ?lineOffset : Float, ?fitSomething : Bool) : flash.text.engine.TextLine;
}
