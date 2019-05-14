package flash.accessibility;

@:require(flash10_1) extern interface ISimpleTextSelection {
	@:flash.property var selectionActiveIndex(get,never) : Int;
	@:flash.property var selectionAnchorIndex(get,never) : Int;
	private function get_selectionActiveIndex() : Int;
	private function get_selectionAnchorIndex() : Int;
}
