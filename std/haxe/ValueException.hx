package haxe;

/**
	An exception containing arbitrary value.

	This class is automatically used for throwing values, which don't extend `haxe.Exception`
	or native exception type.
	For example:
	```haxe
	throw "Terrible error";
	```
	will be compiled to
	```haxe
	throw new ValueException("Terrible error");
	```
**/
class ValueException extends Exception {
	/**
		Thrown value.
	**/
	public var value(default, null):Any;

	public function new(value:Any, ?previous:Exception, ?native:Any):Void {
		super(#if js js.Syntax.code('String({0})', value) #else Std.string(value) #end, previous, native);
		this.value = value;
	}

	/**
		Extract an originally thrown value.

		This method must return the same value on subsequent calls.
		Used internally for catching non-native exceptions.
		Do _not_ override unless you know what you are doing.
	**/
	override function unwrap():Any {
		return value;
	}
}
