package cs.system.data.sqltypes;

/** is a mutable reference type that wraps a  array or a  instance. */
@:native("System.Data.SqlTypes.SqlChars")
extern class SqlChars {
	/**
	 * Returns a null instance of this .
	 * @return An instance whose  property returns . For more information, see Handling
	 * Null Values.
	 */
	static var Null(default, never):cs.system.data.sqltypes.SqlChars;
	/**
	 * Returns a reference to the internal buffer.
	 * @return A reference to the internal buffer. For  instances created on top of
	 * unmanaged pointers, it returns a managed copy of the internal buffer.
	 */
	var Buffer(default, never):cs.NativeArray<cs.Char16>;
	/**
	 * Gets a Boolean value that indicates whether this  is null.
	 * @return if the  is null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the length of the value that is contained in the  instance.
	 * @return A  value that indicates the length in characters of the value that is
	 * contained in the  instance. Returns -1 if no buffer is available to the
	 * instance, or if the value is null. Returns a  for a stream-wrapped instance.
	 */
	var Length(default, never):haxe.Int64;
	/**
	 * Gets the maximum length in two-byte characters of the value the internal buffer
	 * can hold.
	 * @return An  value representing the maximum length in two-byte characters of the
	 * value of the internal buffer. Returns -1 for a stream-wrapped .
	 */
	var MaxLength(default, never):haxe.Int64;
	/**
	 * Returns information about the storage state of this  instance.
	 * @return A  enumeration.
	 */
	var Storage(default, never):cs.system.data.sqltypes.StorageState;
	/**
	 * Returns a managed copy of the value held by this .
	 * @return The value of this  as an array of characters.
	 */
	var Value(default, never):cs.NativeArray<cs.Char16>;
	@:native("get_Item")
	function get_Item(index0:haxe.Int64):cs.Char16;
	@:native("set_Item")
	function set_Item(index0:haxe.Int64, value:cs.Char16):Void;
	@:overload(function():Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	function new(value:cs.system.data.sqltypes.SqlString):Void;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	@:overload(function(value:cs.system.data.sqltypes.SqlChars):cs.system.data.sqltypes.SqlString {})
	/**
	 * Converts a  structure to a  structure.
	 * @param value The  structure to be converted.
	 * @return A  structure.
	 */
	static function op_Explicit(value:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlChars;
	/**
	 * Copies characters from this  instance to the passed-in buffer and returns the
	 * number of copied characters.
	 * @param offset An  value offset into the value that is contained in the 
	 * instance.
	 * @param buffer The character array buffer to copy into.
	 * @param offsetInBuffer An  integer offset into the buffer to start copying into.
	 * @param count An  integer value representing the number of characters to copy.
	 * @return An  value representing the number of copied bytes.
	 */
	function Read(offset:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, offsetInBuffer:Int, count:Int):haxe.Int64;
	/**
	 * Sets the length of this  instance.
	 * @param value The  value representing the length.
	 */
	function SetLength(value:haxe.Int64):Void;
	/** Sets this  instance to null. */
	function SetNull():Void;
	/**
	 * Converts this  instance to its equivalent  representation.
	 * @return A  representation of this type.
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Copies characters from the passed-in buffer to this  instance.
	 * @param offset A  value offset into the value that is contained in the  instance.
	 * @param buffer The character array buffer to copy into.
	 * @param offsetInBuffer An  integer offset into the buffer to start copying into.
	 * @param count An  integer representing the number of characters to copy.
	 */
	function Write(offset:haxe.Int64, buffer:cs.NativeArray<cs.Char16>, offsetInBuffer:Int, count:Int):Void;
}
