package flash.text.ime;

extern interface IIMEClient {
	var compositionEndIndex(default,never) : Int;
	var compositionStartIndex(default,never) : Int;
	var selectionActiveIndex(default,never) : Int;
	var selectionAnchorIndex(default,never) : Int;
	var verticalTextLayout(default,never) : Bool;
	function confirmComposition(?text : String, preserveSelection : Bool = false) : Void;
	function getTextBounds(startIndex : Int, endIndex : Int) : flash.geom.Rectangle;
	function getTextInRange(startIndex : Int, endIndex : Int) : String;
	function selectRange(anchorIndex : Int, activeIndex : Int) : Void;
	function updateComposition(text : String, attributes : flash.Vector<CompositionAttributeRange>, compositionStartIndex : Int, compositionEndIndex : Int) : Void;
}
