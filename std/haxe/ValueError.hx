package haxe;

/**
	An error containing arbitrary value.

	This class is automatically used for throwing non-haxe.Error values like this:
	```haxe
	throw "Terrible error";
	```
**/
class ValueError extends Error {
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Error) {
		super(Std.string(value), previous);
		this.value = value;
	}
}
