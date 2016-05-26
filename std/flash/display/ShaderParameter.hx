package flash.display;

@:final extern class ShaderParameter implements Dynamic {
	var index(default,never) : Int;
	var type(default,never) : ShaderParameterType;
	var value : Array<Dynamic>;
}
