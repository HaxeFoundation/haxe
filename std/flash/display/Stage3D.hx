package flash.display;

@:require(flash11) extern class Stage3D extends flash.events.EventDispatcher {
	@:flash.property var context3D(get,never) : flash.display3D.Context3D;
	@:flash.property var visible(get,set) : Bool;
	@:flash.property var x(get,set) : Float;
	@:flash.property var y(get,set) : Float;
	private function get_context3D() : flash.display3D.Context3D;
	private function get_visible() : Bool;
	private function get_x() : Float;
	private function get_y() : Float;
	function requestContext3D(?context3DRenderMode : String, ?profile : flash.display3D.Context3DProfile) : Void;
	@:require(flash12) function requestContext3DMatchingProfiles(profiles : flash.Vector<String>) : Void;
	private function set_visible(value : Bool) : Bool;
	private function set_x(value : Float) : Float;
	private function set_y(value : Float) : Float;
}
