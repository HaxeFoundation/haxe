package flash.display;

extern class DisplayObjectContainer extends InteractiveObject {
	var mouseChildren : Bool;
	var numChildren(default,never) : Int;
	var tabChildren : Bool;
	var textSnapshot(default,never) : flash.text.TextSnapshot;
	function new() : Void;
	function addChild(child : DisplayObject) : DisplayObject;
	function addChildAt(child : DisplayObject, index : Int) : DisplayObject;
	function areInaccessibleObjectsUnderPoint(point : flash.geom.Point) : Bool;
	function contains(child : DisplayObject) : Bool;
	function getChildAt(index : Int) : DisplayObject;
	function getChildByName(name : String) : DisplayObject;
	function getChildIndex(child : DisplayObject) : Int;
	function getObjectsUnderPoint(point : flash.geom.Point) : Array<DisplayObject>;
	function removeChild(child : DisplayObject) : DisplayObject;
	function removeChildAt(index : Int) : DisplayObject;
	@:require(flash11) function removeChildren(beginIndex : Int = 0, endIndex : Int = 2147483647) : Void;
	function setChildIndex(child : DisplayObject, index : Int) : Void;
	@:require(flash11_8) function stopAllMovieClips() : Void;
	function swapChildren(child1 : DisplayObject, child2 : DisplayObject) : Void;
	function swapChildrenAt(index1 : Int, index2 : Int) : Void;
}
