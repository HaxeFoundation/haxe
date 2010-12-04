package flash.sampler;

@:final extern class NewObjectSample extends Sample {
	var id : Float;
	var object(default,null) : Dynamic;
	var type : Class<Dynamic>;
}
