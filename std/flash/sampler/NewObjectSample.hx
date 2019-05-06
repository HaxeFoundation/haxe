package flash.sampler;

extern final class NewObjectSample extends Sample {
	final id : Float;
	@:flash.property var object(get,never) : Dynamic;
	@:flash.property @:require(flash10_1) var size(get,never) : Float;
	final type : Class<Dynamic>;
	private function get_object() : Dynamic;
	private function get_size() : Float;
}
