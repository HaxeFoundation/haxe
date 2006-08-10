package flash.display;

extern class DisplayObject extends flash.events.EventDispatcher, implements flash.display.IBitmapDrawable {
	function new() : Void;
	var accessibilityProperties : flash.accessibility.AccessibilityProperties;
	var alpha : Float;
	var blendMode : String;
	var cacheAsBitmap : Bool;
	var filters : Array<Dynamic>;
	function getBounds(targetCoordinateSpace : flash.display.DisplayObject) : flash.geom.Rectangle;
	function getRect(targetCoordinateSpace : flash.display.DisplayObject) : flash.geom.Rectangle;
	function globalToLocal(point : flash.geom.Point) : flash.geom.Point;
	var height : Float;
	function hitTestObject(obj : flash.display.DisplayObject) : Bool;
	function hitTestPoint(x : Float, y : Float, ?shapeFlag : Bool) : Bool;
	var loaderInfo(default,null) : flash.display.LoaderInfo;
	function localToGlobal(point : flash.geom.Point) : flash.geom.Point;
	var mask : flash.display.DisplayObject;
	var mouseX(default,null) : Float;
	var mouseY(default,null) : Float;
	var name : String;
	var opaqueBackground : Dynamic;
	var parent(default,null) : flash.display.DisplayObjectContainer;
	var root(default,null) : flash.display.DisplayObject;
	var rotation : Float;
	var scale9Grid : flash.geom.Rectangle;
	var scaleX : Float;
	var scaleY : Float;
	var scrollRect : flash.geom.Rectangle;
	var stage(default,null) : flash.display.Stage;
	var transform : flash.geom.Transform;
	var visible : Bool;
	var width : Float;
	var x : Float;
	var y : Float;
	private function _hitTest(use_xy : Bool, x : Float, y : Float, useShape : Bool, hitTestObject : flash.display.DisplayObject) : Bool;
}
