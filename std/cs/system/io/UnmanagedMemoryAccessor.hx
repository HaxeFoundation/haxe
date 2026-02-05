package cs.system.io;

/** Provides random access to unmanaged blocks of memory from managed code. */
@:native("System.IO.UnmanagedMemoryAccessor")
extern class UnmanagedMemoryAccessor {
	/**
	 * Determines whether the accessor is readable.
	 * @return if the accessor is readable; otherwise, .
	 */
	var CanRead(default, never):Bool;
	/**
	 * Determines whether the accessory is writable.
	 * @return if the accessor is writable; otherwise, .
	 */
	var CanWrite(default, never):Bool;
	/**
	 * Gets the capacity of the accessor.
	 * @return The capacity of the accessor.
	 */
	var Capacity(default, never):haxe.Int64;
	/**
	 * Determines whether the accessor is currently open by a process.
	 * @return if the accessor is open; otherwise, .
	 */
	var IsOpen(default, never):Bool;
	@:overload(function(buffer:cs.system.runtime.interopservices.SafeBuffer, offset:haxe.Int64, capacity:haxe.Int64):Void {})
	function new(buffer:cs.system.runtime.interopservices.SafeBuffer, offset:haxe.Int64, capacity:haxe.Int64, access:cs.system.io.FileAccess):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Reads a structure of type  from the accessor into a provided reference.
	 * @param T The type of structure.
	 * @param position The position in the accessor at which to begin reading.
	 * @param structure The structure to contain the read data.
	 */
	function Read<T>(position:haxe.Int64, structure:cs.Ref<T>):Void;
	/**
	 * Reads structures of type  from the accessor into an array of type .
	 * @param T The type of structure.
	 * @param position The number of bytes in the accessor at which to begin reading.
	 * @param array The array to contain the structures read from the accessor.
	 * @param offset The index in  in which to place the first copied structure.
	 * @param count The number of structures of type T to read from the accessor.
	 * @return The number of structures read into . This value can be less than  if
	 * there are fewer structures available, or zero if the end of the accessor is
	 * reached.
	 */
	function ReadArray<T>(position:haxe.Int64, array:cs.NativeArray<T>, offset:Int, count:Int):Int;
	/**
	 * Reads a Boolean value from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return or .
	 */
	function ReadBoolean(position:haxe.Int64):Bool;
	/**
	 * Reads a byte value from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadByte(position:haxe.Int64):cs.UInt8;
	/**
	 * Reads a character from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadChar(position:haxe.Int64):cs.Char16;
	/**
	 * Reads a decimal value from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadDecimal(position:haxe.Int64):cs.system.Decimal;
	/**
	 * Reads a double-precision floating-point value from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadDouble(position:haxe.Int64):Float;
	/**
	 * Reads a 16-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadInt16(position:haxe.Int64):cs.Int16;
	/**
	 * Reads a 32-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadInt32(position:haxe.Int64):Int;
	/**
	 * Reads a 64-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadInt64(position:haxe.Int64):haxe.Int64;
	/**
	 * Reads an 8-bit signed integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadSByte(position:haxe.Int64):cs.Int8;
	/**
	 * Reads a single-precision floating-point value from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadSingle(position:haxe.Int64):Single;
	/**
	 * Reads an unsigned 16-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadUInt16(position:haxe.Int64):cs.UInt16;
	/**
	 * Reads an unsigned 32-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadUInt32(position:haxe.Int64):cs.UInt;
	/**
	 * Reads an unsigned 64-bit integer from the accessor.
	 * @param position The number of bytes into the accessor at which to begin reading.
	 * @return The value that was read.
	 */
	function ReadUInt64(position:haxe.Int64):cs.UInt64;
	@:overload(function(position:haxe.Int64, value:Bool):Void {})
	@:overload(function(position:haxe.Int64, value:cs.UInt8):Void {})
	@:overload(function(position:haxe.Int64, value:cs.Char16):Void {})
	@:overload(function(position:haxe.Int64, value:cs.system.Decimal):Void {})
	@:overload(function(position:haxe.Int64, value:Float):Void {})
	@:overload(function(position:haxe.Int64, value:cs.Int16):Void {})
	@:overload(function(position:haxe.Int64, value:Int):Void {})
	@:overload(function(position:haxe.Int64, value:haxe.Int64):Void {})
	@:overload(function(position:haxe.Int64, value:cs.Int8):Void {})
	@:overload(function(position:haxe.Int64, value:Single):Void {})
	@:overload(function(position:haxe.Int64, value:cs.UInt16):Void {})
	@:overload(function(position:haxe.Int64, value:cs.UInt):Void {})
	@:overload(function(position:haxe.Int64, value:cs.UInt64):Void {})
	/**
	 * Writes a Boolean value into the accessor.
	 * @param position The number of bytes into the accessor at which to begin writing.
	 * @param value The value to write.
	 */
	function Write<T>(position:haxe.Int64, structure:cs.Ref<T>):Void;
	/**
	 * Writes structures from an array of type  into the accessor.
	 * @param T The type of structure.
	 * @param position The number of bytes into the accessor at which to begin writing.
	 * @param array The array to write into the accessor.
	 * @param offset The index in  to start writing from.
	 * @param count The number of structures in  to write.
	 */
	function WriteArray<T>(position:haxe.Int64, array:cs.NativeArray<T>, offset:Int, count:Int):Void;
}
