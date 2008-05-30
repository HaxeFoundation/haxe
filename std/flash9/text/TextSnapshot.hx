package flash.text;

extern class TextSnapshot {
	var charCount(default,null) : Int;
	function new() : Void;
	function findText(beginIndex : Int, textToFind : String, caseSensitive : Bool) : Int;
	function getSelected(beginIndex : Int, endIndex : Int) : Bool;
	function getSelectedText(?includeLineEndings : Bool) : String;
	function getText(beginIndex : Int, endIndex : Int, ?includeLineEndings : Bool) : String;
	function getTextRunInfo(beginIndex : Int, endIndex : Int) : Array<Dynamic>;
	function hitTestTextNearPos(x : Float, y : Float, ?maxDistance : Float) : Float;
	function setSelectColor(?hexColor : UInt) : Void;
	function setSelected(beginIndex : Int, endIndex : Int, select : Bool) : Void;
}
