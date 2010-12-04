package flash.display;

@:final extern class ShaderParameter implements Dynamic {
	var index(default,null) : Int;
	var type(default,null) : ShaderParameterType;
	var value : Array<Dynamic>;
}
