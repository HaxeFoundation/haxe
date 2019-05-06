package flash.display;

extern class DisplayObjectContainer extends InteractiveObject {
	@:flash.property var mouseChildren(get,set) : Bool;
	@:flash.property var numChildren(get,never) : Int;
	@:flash.property var tabChildren(get,set) : Bool;
	@:flash.property var textSnapshot(get,never) : flash.text.TextSnapshot;
	function new() : Void;
	function addChild(child : DisplayObject) : DisplayObject;
	function addChildAt(child : DisplayObject, index : Int) : DisplayObject;
	function areInaccessibleObjectsUnderPoint(point : flash.geom.Point) : Bool;
	function contains(child : DisplayObject) : Bool;
	function getChildAt(index : Int) : DisplayObject;
	function getChildByName(name : String) : DisplayObject;
	function getChildIndex(child : DisplayObject) : Int;
	function getObjectsUnderPoint(point : flash.geom.Point) : Array<DisplayObject>;
	private function get_mouseChildren() : Bool;
	private function get_numChildren() : Int;
	private function get_tabChildren() : Bool;
	private function get_textSnapshot() : flash.text.TextSnapshot;
	function removeChild(child : DisplayObject) : DisplayObject;
	function removeChildAt(index : Int) : DisplayObject;
	@:require(flash11) function removeChildren(beginIndex : Int = 0, endIndex : Int = 2147483647) : Void;
	function setChildIndex(child : DisplayObject, index : Int) : Void;
	private function set_mouseChildren(value : Bool) : Bool;
	private function set_tabChildren(value : Bool) : Bool;
	@:require(flash11_8) function stopAllMovieClips() : Void;
	function swapChildren(child1 : DisplayObject, child2 : DisplayObject) : Void;
	function swapChildrenAt(index1 : Int, index2 : Int) : Void;
}
