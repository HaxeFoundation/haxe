package flash.display;

@:require(flash11) extern class Stage3D extends flash.events.EventDispatcher {
	var context3D(default,never) : flash.display3D.Context3D;
	var visible : Bool;
	var x : Float;
	var y : Float;
	function requestContext3D(?context3DRenderMode : String, ?profile : flash.display3D.Context3DProfile) : Void;
	@:require(flash12) function requestContext3DMatchingProfiles(profiles : flash.Vector<String>) : Void;
}
