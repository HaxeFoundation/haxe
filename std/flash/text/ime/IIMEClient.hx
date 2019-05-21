package flash.text.ime;

extern interface IIMEClient {
	@:flash.property var compositionEndIndex(get,never) : Int;
	@:flash.property var compositionStartIndex(get,never) : Int;
	@:flash.property var selectionActiveIndex(get,never) : Int;
	@:flash.property var selectionAnchorIndex(get,never) : Int;
	@:flash.property var verticalTextLayout(get,never) : Bool;
	function confirmComposition(?text : String, preserveSelection : Bool = false) : Void;
	function getTextBounds(startIndex : Int, endIndex : Int) : flash.geom.Rectangle;
	function getTextInRange(startIndex : Int, endIndex : Int) : String;
	private function get_compositionEndIndex() : Int;
	private function get_compositionStartIndex() : Int;
	private function get_selectionActiveIndex() : Int;
	private function get_selectionAnchorIndex() : Int;
	private function get_verticalTextLayout() : Bool;
	function selectRange(anchorIndex : Int, activeIndex : Int) : Void;
	function updateComposition(text : String, attributes : flash.Vector<CompositionAttributeRange>, compositionStartIndex : Int, compositionEndIndex : Int) : Void;
}
