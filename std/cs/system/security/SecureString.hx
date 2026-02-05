package cs.system.security;

/** Represents text that should be kept confidential, such as by deleting it from computer memory when no longer needed. This class cannot be inherited. */
@:native("System.Security.SecureString")
extern class SecureString {
	/**
	 * Gets the number of characters in the current secure string.
	 * @return The number of  objects in this secure string.
	 */
	var Length(default, never):Int;
	@:overload(function():Void {})
	function new(value:cs.Pointer<cs.Char16>, length:Int):Void;
	/**
	 * Appends a character to the end of the current secure string.
	 * @param c A character to append to this secure string.
	 */
	function AppendChar(c:cs.Char16):Void;
	/** Deletes the value of the current secure string. */
	function Clear():Void;
	/**
	 * Creates a copy of the current secure string.
	 * @return A duplicate of this secure string.
	 */
	function Copy():cs.system.security.SecureString;
	/** Releases all resources used by the current  object. */
	function Dispose():Void;
	/**
	 * Inserts a character in this secure string at the specified index position.
	 * @param index The index position where parameter  is inserted.
	 * @param c The character to insert.
	 */
	function InsertAt(index:Int, c:cs.Char16):Void;
	/**
	 * Indicates whether this secure string is marked read-only.
	 * @return if this secure string is marked read-only; otherwise, .
	 */
	function IsReadOnly():Bool;
	/** Makes the text value of this secure string read-only. */
	function MakeReadOnly():Void;
	/**
	 * Removes the character at the specified index position from this secure string.
	 * @param index The index position of a character in this secure string.
	 */
	function RemoveAt(index:Int):Void;
	/**
	 * Replaces the existing character at the specified index position with another
	 * character.
	 * @param index The index position of an existing character in this secure string
	 * @param c A character that replaces the existing character.
	 */
	function SetAt(index:Int, c:cs.Char16):Void;
}
