package cs.system.data.sqltypes;

/** Represents a mutable reference type that wraps either a  or a . */
@:native("System.Data.SqlTypes.SqlBytes")
extern class SqlBytes {
	/**
	 * Gets a null instance of this .
	 * @return An instance whose  property returns .
	 */
	static var Null(default, never):cs.system.data.sqltypes.SqlBytes;
	/**
	 * Returns a reference to the internal buffer.
	 * @return A reference to the internal buffer. For  instances created on top of
	 * unmanaged pointers, it returns a managed copy of the internal buffer.
	 */
	var Buffer(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets a Boolean value that indicates whether this  is null.
	 * @return if the  is null,  otherwise.
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the length of the value that is contained in the  instance.
	 * @return A  value representing the length of the value that is contained in the 
	 * instance. Returns -1 if no buffer is available to the instance or if the value
	 * is null. Returns a  for a stream-wrapped instance.
	 */
	var Length(default, never):haxe.Int64;
	/**
	 * Gets the maximum length of the value of the internal buffer of this .
	 * @return A long representing the maximum length of the value of the internal
	 * buffer. Returns -1 for a stream-wrapped .
	 */
	var MaxLength(default, never):haxe.Int64;
	/**
	 * Returns information about the storage state of this  instance.
	 * @return A  enumeration.
	 */
	var Storage(default, never):cs.system.data.sqltypes.StorageState;
	/**
	 * Gets or sets the data of this  as a stream.
	 * @return The stream that contains the SqlBytes data.
	 */
	var Stream(default, default):cs.system.io.Stream;
	/**
	 * Returns a managed copy of the value held by this .
	 * @return The value of this  as an array of bytes.
	 */
	var Value(default, never):cs.NativeArray<cs.UInt8>;
	@:native("get_Item")
	function get_Item(index0:haxe.Int64):cs.UInt8;
	@:native("set_Item")
	function set_Item(index0:haxe.Int64, value:cs.UInt8):Void;
	@:overload(function():Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(value:cs.system.data.sqltypes.SqlBinary):Void {})
	function new(s:cs.system.io.Stream):Void;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	@:overload(function(value:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBytes {})
	/**
	 * Converts a  structure to a  structure.
	 * @param value The  structure to be converted.
	 * @return A  structure.
	 */
	static function op_Explicit(value:cs.system.data.sqltypes.SqlBytes):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Copies bytes from this  instance to the passed-in buffer and returns the number
	 * of copied bytes.
	 * @param offset An  long value offset into the value that is contained in the 
	 * instance.
	 * @param buffer The byte array buffer to copy into.
	 * @param offsetInBuffer An  integer offset into the buffer to start copying into.
	 * @param count An  integer representing the number of bytes to copy.
	 * @return An  long value representing the number of copied bytes.
	 */
	function Read(offset:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, offsetInBuffer:Int, count:Int):haxe.Int64;
	/**
	 * Sets the length of this  instance.
	 * @param value The  long value representing the length.
	 */
	function SetLength(value:haxe.Int64):Void;
	/** Sets this  instance to null. */
	function SetNull():Void;
	/**
	 * Constructs and returns a  from this  instance.
	 * @return A  from this instance.
	 */
	function ToSqlBinary():cs.system.data.sqltypes.SqlBinary;
	/**
	 * Copies bytes from the passed-in buffer to this  instance.
	 * @param offset An  long value offset into the value that is contained in the 
	 * instance.
	 * @param buffer The byte array buffer to copy into.
	 * @param offsetInBuffer An  integer offset into the buffer to start copying into.
	 * @param count An  integer representing the number of bytes to copy.
	 */
	function Write(offset:haxe.Int64, buffer:cs.NativeArray<cs.UInt8>, offsetInBuffer:Int, count:Int):Void;
}
