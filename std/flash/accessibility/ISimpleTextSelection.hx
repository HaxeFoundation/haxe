package flash.accessibility;

@:require(flash10_1) extern interface ISimpleTextSelection {
	var selectionActiveIndex(get,never) : Int;
	var selectionAnchorIndex(get,never) : Int;
	private function get_selectionActiveIndex() : Int;
	private function get_selectionAnchorIndex() : Int;
}
