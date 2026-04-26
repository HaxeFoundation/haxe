package cpp;

@:include("hx/thread/Scratch.hpp")
@:semantics(value)
@:cpp.ValueType({ namespace : [ "hx", "thread" ], flags : [ StackOnly ] })
private extern class Scratch {
	final view : cpp.marshal.View<cpp.UInt8>;

	static function alloc(bytes:Int):Scratch;
}