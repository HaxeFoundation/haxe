package haxe;

/**
	Data type used to indicate the absence of a value instead of `Void` in
	value-places in types with type parameters.
**/
enum abstract NoData(Null<Dynamic>) from Dynamic {
	var NoData = null;
}