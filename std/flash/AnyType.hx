package flash;

/**
	This type represents the Flash `*` type, which is
	actually the absense of type. It can be used as a
	type parameter for `flash.Vector` to represent the
	native `Vector.<*>` type.
**/
@:coreType abstract AnyType from Dynamic to Dynamic {}
