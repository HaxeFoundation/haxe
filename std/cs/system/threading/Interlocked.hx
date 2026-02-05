package cs.system.threading;

/** Provides atomic operations for variables that are shared by multiple threads. */
@:native("System.Threading.Interlocked")
extern class Interlocked {
	@:overload(function(location1:cs.Ref<Int>, value:Int):Int {})
	/**
	 * Adds two 32-bit integers and replaces the first integer with the sum, as an
	 * atomic operation.
	 * @param location1 A variable containing the first value to be added. The sum of
	 * the two values is stored in .
	 * @param value The value to be added to the integer at .
	 * @return The new value stored at .
	 */
	static function Add(location1:cs.Ref<haxe.Int64>, value:haxe.Int64):haxe.Int64;
	@:overload(function(location1:cs.Ref<Float>, value:Float, comparand:Float):Float {})
	@:overload(function(location1:cs.Ref<Int>, value:Int, comparand:Int):Int {})
	@:overload(function(location1:cs.Ref<haxe.Int64>, value:haxe.Int64, comparand:haxe.Int64):haxe.Int64 {})
	@:overload(function(location1:cs.Ref<cs.system.IntPtr>, value:cs.system.IntPtr, comparand:cs.system.IntPtr):cs.system.IntPtr {})
	@:overload(function(location1:cs.Ref<Dynamic>, value:Dynamic, comparand:Dynamic):Dynamic {})
	@:overload(function(location1:cs.Ref<Single>, value:Single, comparand:Single):Single {})
	/**
	 * Compares two double-precision floating point numbers for equality and, if they
	 * are equal, replaces the first value.
	 * @param location1 The destination, whose value is compared with  and possibly
	 * replaced.
	 * @param value The value that replaces the destination value if the comparison
	 * results in equality.
	 * @param comparand The value that is compared to the value at .
	 * @return The original value in .
	 */
	static function CompareExchange<T>(location1:cs.Ref<T>, value:T, comparand:T):T;
	@:overload(function(location:cs.Ref<Int>):Int {})
	/**
	 * Decrements a specified variable and stores the result, as an atomic operation.
	 * @param location The variable whose value is to be decremented.
	 * @return The decremented value.
	 */
	static function Decrement(location:cs.Ref<haxe.Int64>):haxe.Int64;
	@:overload(function(location1:cs.Ref<Float>, value:Float):Float {})
	@:overload(function(location1:cs.Ref<Int>, value:Int):Int {})
	@:overload(function(location1:cs.Ref<haxe.Int64>, value:haxe.Int64):haxe.Int64 {})
	@:overload(function(location1:cs.Ref<cs.system.IntPtr>, value:cs.system.IntPtr):cs.system.IntPtr {})
	@:overload(function(location1:cs.Ref<Dynamic>, value:Dynamic):Dynamic {})
	@:overload(function(location1:cs.Ref<Single>, value:Single):Single {})
	/**
	 * Sets a double-precision floating point number to a specified value and returns
	 * the original value, as an atomic operation.
	 * @param location1 The variable to set to the specified value.
	 * @param value The value to which the  parameter is set.
	 * @return The original value of .
	 */
	static function Exchange<T>(location1:cs.Ref<T>, value:T):T;
	@:overload(function(location:cs.Ref<Int>):Int {})
	/**
	 * Increments a specified variable and stores the result, as an atomic operation.
	 * @param location The variable whose value is to be incremented.
	 * @return The incremented value.
	 */
	static function Increment(location:cs.Ref<haxe.Int64>):haxe.Int64;
	/** Synchronizes memory access as follows: The processor that executes the current thread cannot reorder instructions in such a way that memory accesses before the call to  execute after memory accesses that follow the call to . */
	static function MemoryBarrier():Void;
	/** Provides a process-wide memory barrier that ensures that reads and writes from any CPU cannot move across the barrier. */
	static function MemoryBarrierProcessWide():Void;
	/**
	 * Returns a 64-bit value, loaded as an atomic operation.
	 * @param location The 64-bit value to be loaded.
	 * @return The loaded value.
	 */
	static function Read(location:cs.Ref<haxe.Int64>):haxe.Int64;
}
