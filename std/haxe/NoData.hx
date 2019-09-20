package haxe;

/**
	Data type used to indicate the absence of a value, especially in types with
	type parameters.
**/
abstract NoData(Int) {
	public inline function new()
		this = 0;
}
