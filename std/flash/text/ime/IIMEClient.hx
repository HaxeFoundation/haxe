package flash.text.ime;

extern interface IIMEClient {
	var compositionEndIndex(default,null) : Int;
	var compositionStartIndex(default,null) : Int;
	var selectionActiveIndex(default,null) : Int;
	var selectionAnchorIndex(default,null) : Int;
	var verticalTextLayout(default,null) : Bool;
	function confirmComposition(?text : String, preserveSelection : Bool = false) : Void;
	function getTextBounds(startIndex : Int, endIndex : Int) : flash.geom.Rectangle;
	function getTextInRange(startIndex : Int, endIndex : Int) : String;
	function selectRange(anchorIndex : Int, activeIndex : Int) : Void;
	function updateComposition(text : String, attributes : flash.Vector<CompositionAttributeRange>, compositionStartIndex : Int, compositionEndIndex : Int) : Void;
}
