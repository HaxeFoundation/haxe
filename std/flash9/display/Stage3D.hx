package flash.display;

@:require(flash11) extern class Stage3D extends flash.events.EventDispatcher {
	var context3D(default,null) : flash.display3D.Context3D;
	var viewPort : flash.geom.Rectangle;
	function requestContext3D(?context3DRenderMode : String) : Void;
}
