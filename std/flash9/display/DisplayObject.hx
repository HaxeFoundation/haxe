package flash.display;

extern class DisplayObject extends flash.events.EventDispatcher, implements IBitmapDrawable {
	var accessibilityProperties : flash.accessibility.AccessibilityProperties;
	var alpha : Float;
	var blendMode : BlendMode;
	var cacheAsBitmap : Bool;
	var filters : Array<Dynamic>; // does not use variance here
	function getBounds(targetCoordinateSpace : DisplayObject) : flash.geom.Rectangle;
	function getRect(targetCoordinateSpace : DisplayObject) : flash.geom.Rectangle;
	function globalToLocal(point : flash.geom.Point) : flash.geom.Point;
	var height : Float;
	function hitTestObject(obj : DisplayObject) : Bool;
	function hitTestPoint(x : Float, y : Float, ?shapeFlag : Bool) : Bool;
	var loaderInfo(default,null) : LoaderInfo;
	function localToGlobal(point : flash.geom.Point) : flash.geom.Point;
	var mask : DisplayObject;
	var mouseX(default,null) : Float;
	var mouseY(default,null) : Float;
	var name : String;
	var opaqueBackground : Null<UInt>;
	var parent(default,null) : DisplayObjectContainer;
	var root(default,null) : DisplayObject;
	var rotation : Float;
	var scale9Grid : flash.geom.Rectangle;
	var scaleX : Float;
	var scaleY : Float;
	var scrollRect : flash.geom.Rectangle;
	var stage(default,null) : Stage;
	var transform : flash.geom.Transform;
	var visible : Bool;
	var width : Float;
	var x : Float;
	var y : Float;
	private function _hitTest(use_xy : Bool, x : Float, y : Float, useShape : Bool, hitTestObject : flash.display.DisplayObject) : Bool;
}
