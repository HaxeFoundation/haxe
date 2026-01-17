package cs.system.threading;

/**
	Extern for System.Threading.Interlocked class providing atomic operations.
**/
@:native("System.Threading.Interlocked")
extern class Interlocked {
	/**
		Adds two 32-bit integers and replaces the first integer with the sum, as an atomic operation.
		Returns the new value stored at the first integer.
	**/
	@:overload(function(location1:cs.Ref<Int>, value:Int):Int {})
	static function Add(location1:Int, value:Int):Int;

	/**
		Compares two 32-bit signed integers for equality and, if they are equal, replaces the first value.
		Returns the original value in location1.
	**/
	@:overload(function<T:{}>(location1:cs.Ref<T>, value:T, comparand:T):T {})
	@:overload(function(location1:cs.Ref<Int>, value:Int, comparand:Int):Int {})
	static function CompareExchange(location1:Int, value:Int, comparand:Int):Int;

	/**
		Sets a 32-bit signed integer to a specified value and returns the original value, as an atomic operation.
	**/
	@:overload(function<T:{}>(location1:cs.Ref<T>, value:T):T {})
	@:overload(function(location1:cs.Ref<Int>, value:Int):Int {})
	static function Exchange(location1:Int, value:Int):Int;

	/**
		Returns a 64-bit value, loaded as an atomic operation.
	**/
	static function Read(location:cs.Ref<haxe.Int64>):haxe.Int64;

	/**
		Increments a specified variable and stores the result, as an atomic operation.
	**/
	@:overload(function(location:cs.Ref<Int>):Int {})
	static function Increment(location:Int):Int;

	/**
		Decrements a specified variable and stores the result, as an atomic operation.
	**/
	@:overload(function(location:cs.Ref<Int>):Int {})
	static function Decrement(location:Int):Int;
}
