package haxe;

/**
	Data type used to indicate the absence of a value, especially in types with
	type parameters.
**/
enum abstract NoData(Null<Dynamic>) from Dynamic {
	var NoData = null;
}
