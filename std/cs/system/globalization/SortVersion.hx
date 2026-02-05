package cs.system.globalization;

/** Provides information about the version of Unicode used to compare and order strings. */
@:native("System.Globalization.SortVersion")
extern class SortVersion {
	/**
	 * Gets the full version number of the  object.
	 * @return The version number of this  object.
	 */
	var FullVersion(default, never):Int;
	/**
	 * Gets a globally unique identifier for this  object.
	 * @return A globally unique identifier for this  object.
	 */
	var SortId(default, never):cs.system.Guid;
	function new(fullVersion:Int, sortId:cs.system.Guid):Void;
	/**
	 * Indicates whether two  instances are equal.
	 * @param left The first instance to compare.
	 * @param right The second instance to compare.
	 * @return if the values of  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.globalization.SortVersion, right:cs.system.globalization.SortVersion):Bool;
	/**
	 * Indicates whether two  instances are not equal.
	 * @param left The first instance to compare.
	 * @param right The second instance to compare.
	 * @return if the values of  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.globalization.SortVersion, right:cs.system.globalization.SortVersion):Bool;
	@:overload(function(other:cs.system.globalization.SortVersion):Bool {})
	/**
	 * Returns a value that indicates whether this  instance is equal to a specified 
	 * object.
	 * @param other The object to compare with this instance.
	 * @return if  represents the same version as this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
