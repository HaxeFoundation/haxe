package cs.system.threading;

/** Contains methods for performing volatile memory operations. */
@:native("System.Threading.Volatile")
extern class Volatile {
	@:overload(function(location:cs.Ref<Bool>):Bool {})
	@:overload(function(location:cs.Ref<cs.UInt8>):cs.UInt8 {})
	@:overload(function(location:cs.Ref<Float>):Float {})
	@:overload(function(location:cs.Ref<cs.Int16>):cs.Int16 {})
	@:overload(function(location:cs.Ref<Int>):Int {})
	@:overload(function(location:cs.Ref<haxe.Int64>):haxe.Int64 {})
	@:overload(function(location:cs.Ref<cs.system.IntPtr>):cs.system.IntPtr {})
	@:overload(function(location:cs.Ref<cs.Int8>):cs.Int8 {})
	@:overload(function(location:cs.Ref<Single>):Single {})
	@:overload(function(location:cs.Ref<cs.UInt16>):cs.UInt16 {})
	@:overload(function(location:cs.Ref<cs.UInt>):cs.UInt {})
	@:overload(function(location:cs.Ref<cs.UInt64>):cs.UInt64 {})
	@:overload(function(location:cs.Ref<cs.system.UIntPtr>):cs.system.UIntPtr {})
	/**
	 * Reads the value of the specified field. On systems that require it, inserts a
	 * memory barrier that prevents the processor from reordering memory operations as
	 * follows: If a read or write appears after this method in the code, the processor
	 * cannot move it before this method.
	 * @param location The field to read.
	 * @return The value that was read. This value is the latest written by any
	 * processor in the computer, regardless of the number of processors or the state
	 * of processor cache.
	 */
	static function Read<T>(location:cs.Ref<T>):T;
	@:overload(function(location:cs.Ref<Bool>, value:Bool):Void {})
	@:overload(function(location:cs.Ref<cs.UInt8>, value:cs.UInt8):Void {})
	@:overload(function(location:cs.Ref<Float>, value:Float):Void {})
	@:overload(function(location:cs.Ref<cs.Int16>, value:cs.Int16):Void {})
	@:overload(function(location:cs.Ref<Int>, value:Int):Void {})
	@:overload(function(location:cs.Ref<haxe.Int64>, value:haxe.Int64):Void {})
	@:overload(function(location:cs.Ref<cs.system.IntPtr>, value:cs.system.IntPtr):Void {})
	@:overload(function(location:cs.Ref<cs.Int8>, value:cs.Int8):Void {})
	@:overload(function(location:cs.Ref<Single>, value:Single):Void {})
	@:overload(function(location:cs.Ref<cs.UInt16>, value:cs.UInt16):Void {})
	@:overload(function(location:cs.Ref<cs.UInt>, value:cs.UInt):Void {})
	@:overload(function(location:cs.Ref<cs.UInt64>, value:cs.UInt64):Void {})
	@:overload(function(location:cs.Ref<cs.system.UIntPtr>, value:cs.system.UIntPtr):Void {})
	/**
	 * Writes the specified value to the specified field. On systems that require it,
	 * inserts a memory barrier that prevents the processor from reordering memory
	 * operations as follows: If a read or write appears before this method in the
	 * code, the processor cannot move it after this method.
	 * @param location The field where the value is written.
	 * @param value The value to write. The value is written immediately so that it is
	 * visible to all processors in the computer.
	 */
	static function Write<T>(location:cs.Ref<T>, value:T):Void;
}
