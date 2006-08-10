package flash.display;

extern class DisplayObjectContainer extends flash.display.InteractiveObject {
	function new() : Void;
	function addChild(child : flash.display.DisplayObject) : flash.display.DisplayObject;
	function addChildAt(child : flash.display.DisplayObject, index : Int) : flash.display.DisplayObject;
	function areInaccessibleObjectsUnderPoint(point : flash.geom.Point) : Bool;
	function contains(child : flash.display.DisplayObject) : Bool;
	function getChildAt(index : Int) : flash.display.DisplayObject;
	function getChildByName(name : String) : flash.display.DisplayObject;
	function getChildIndex(child : flash.display.DisplayObject) : Int;
	function getObjectsUnderPoint(point : flash.geom.Point) : Array<Dynamic>;
	var mouseChildren : Bool;
	var numChildren(default,null) : Int;
	function removeChild(child : flash.display.DisplayObject) : flash.display.DisplayObject;
	function removeChildAt(index : Int) : flash.display.DisplayObject;
	function setChildIndex(child : flash.display.DisplayObject, index : Int) : Void;
	function swapChildren(child1 : flash.display.DisplayObject, child2 : flash.display.DisplayObject) : Void;
	function swapChildrenAt(index1 : Int, index2 : Int) : Void;
	var tabChildren : Bool;
	var textSnapshot(default,null) : flash.text.TextSnapshot;
}
