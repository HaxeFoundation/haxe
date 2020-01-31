package haxe;

/**
	An error containing arbitrary value.

	This class is automatically used for throws like the following:
	```haxe
	throw "Terrible error";
	```
**/
@:keep
class ValueError extends Error {
	public var value(default,null):Any;

	public function new(value:Any, ?native:haxe.Error.NativeException) {
		super(Std.string(value), native);
		this.value = value;
	}
}
