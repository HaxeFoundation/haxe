package flash.sampler;

extern final class NewObjectSample extends Sample {
	final id : Float;
	var object(get,never) : Dynamic;
	@:require(flash10_1) var size(get,never) : Float;
	final type : Class<Dynamic>;
	private function get_object() : Dynamic;
	private function get_size() : Float;
}
