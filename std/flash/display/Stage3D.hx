package flash.display;

@:require(flash11) extern class Stage3D extends flash.events.EventDispatcher {
	var context3D(default,null) : flash.display3D.Context3D;
	var visible : Bool;
	var x : Float;
	var y : Float;
	function requestContext3D(?context3DRenderMode : flash.display3D.Context3DRenderMode, ?profile : flash.display3D.Context3DProfile) : Void;
	@:require(flash12) function requestContext3DMatchingProfiles(profiles : flash.Vector<String>) : Void;
}
