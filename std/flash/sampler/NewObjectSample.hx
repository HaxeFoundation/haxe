package flash.sampler;

@:final extern class NewObjectSample extends Sample {
	var id(default,never) : Float;
	var object(default,never) : Dynamic;
	@:require(flash10_1) var size(default,never) : Float;
	var type(default,never) : Class<Dynamic>;
}
