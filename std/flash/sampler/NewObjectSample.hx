package flash.sampler;

@:final extern class NewObjectSample extends Sample {
	var id : Float;
	var object(default,null) : Dynamic;
	@:require(flash10_1) var size(default,null) : Float;
	var type : Class<Dynamic>;
}
