package cs.system.io;

@:native("System.IO.EnumerationOptions")
extern class EnumerationOptions {
	/**
	 * Gets or sets the attributes to skip. The default is FileAttributes.Hidden |
	 * FileAttributes.System.
	 * @return The attributes to skip.
	 */
	var AttributesToSkip(default, default):cs.system.io.FileAttributes;
	/**
	 * Gets or sets the suggested buffer size, in bytes. The default is 0 (no
	 * suggestion).
	 * @return The buffer size.
	 */
	var BufferSize(default, default):Int;
	/**
	 * Gets or sets a value that indicates whether to skip files or directories when
	 * access is denied (for example,  or ). The default is .
	 * @return to skip innacessible files or directories; otherwise, .
	 */
	var IgnoreInaccessible(default, default):Bool;
	/**
	 * Gets or sets the case matching behavior.
	 * @return One of the enumeration values that indicates the case matching behavior.
	 */
	var MatchCasing(default, default):cs.system.io.MatchCasing;
	/**
	 * Gets or sets the match type.
	 * @return One of the enumeration values that indicates the match type.
	 */
	var MatchType(default, default):cs.system.io.MatchType;
	/**
	 * Gets or sets a value that indicates whether to recurse into subdirectories while
	 * enumerating. The default is .
	 * @return to recurse into subdirectories; otherwise, .
	 */
	var RecurseSubdirectories(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether to return the special directory
	 * entries "." and "..".
	 * @return to return the special directory entries "." and ".."; otherwise, .
	 */
	var ReturnSpecialDirectories(default, default):Bool;
	function new():Void;
}
