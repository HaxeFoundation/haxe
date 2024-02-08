package js.lib;

private typedef E<A, B> = haxe.extern.EitherType<A, B>;
private typedef IntTypedArray = E<Int8Array, E<Uint8Array, E<Int16Array, E<Uint16Array, E<Int32Array, Uint32Array>>>>>;

/**
	The Atomics object provides atomic operations as static methods. They are used with SharedArrayBuffer and ArrayBuffer objects.

	Documentation [Atomics](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Atomics) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Atomics/contributors.txt), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Atomics")
extern class Atomics {
	/**
		Adds the provided value to the existing value at the specified index of the array.
		Returns the old value at that index.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function add(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		Computes a bitwise AND on the value at the specified index of the array with the provided value.
		Returns the old value at that index.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function and(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		Stores a value at the specified index of the array, if it equals a value.
		Returns the old value.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function compareExchange(typedArray:IntTypedArray, index:Int, expectedValue:Int, replacementValue:Int):Int;

	/**
		Stores a value at the specified index of the array.
		Returns the old value.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function exchange(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		An optimization primitive that can be used to determine whether to use locks or atomic operations.
		Returns `true` if an atomic operation on arrays of the given element size will be implemented using a hardware atomic operation (as opposed to a lock). Experts only.
	**/
	static function isLockFree(size:Int):Bool;

	/**
		Returns the value at the specified index of the array.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function load(typedArray:IntTypedArray, index:Int):Int;

	/**
		Notifies agents that are waiting on the specified index of the array.
		Returns the number of agents that were notified.
	**/
	static function notify(typedArray:IntTypedArray, index:Int, ?count:Int):Int;

	/**
		Computes a bitwise OR on the value at the specified index of the array with the provided value.
		Returns the old value at that index.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function or(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		Stores a value at the specified index of the array.
		Returns the value.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function store(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		Subtracts a value at the specified index of the array.
		Returns the old value at that index.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function sub(typedArray:IntTypedArray, index:Int, value:Int):Int;

	/**
		Verifies that the specified index of the array still contains a value and sleeps awaiting or times out.
		Returns either "ok", "not-equal", or "timed-out". If waiting is not allowed in the calling agent then it throws an Error exception.
		Most browsers will not allow wait() on the browser's main thread.)
	**/
	static function wait(typedArray:Int32Array, index:Int, value:Int, ?timeout:Int):WaitValue;

	/**
		Computes a bitwise XOR on the value at the specified index of the array with the provided value.
		Returns the old value at that index.
		This atomic operation guarantees that no other write happens until the modified value is written back.
	**/
	static function xor(typedArray:IntTypedArray, index:Int, value:Int):Int;
}

enum abstract WaitValue(String) {
	var OK = "ok";
	var NotEqual = "not-equal";
	var TimedOut = "timed-out";
}
