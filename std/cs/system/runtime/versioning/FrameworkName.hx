package cs.system.runtime.versioning;

/** Represents the name of a version of the .NET Framework. */
@:native("System.Runtime.Versioning.FrameworkName")
extern class FrameworkName {
	/**
	 * Gets the full name of this  object.
	 * @return The full name of this  object.
	 */
	var FullName(default, never):String;
	/**
	 * Gets the identifier of this  object.
	 * @return The identifier of this  object.
	 */
	var Identifier(default, never):String;
	/**
	 * Gets the profile name of this  object.
	 * @return The profile name of this  object.
	 */
	var Profile(default, never):String;
	/**
	 * Gets the version of this  object.
	 * @return An object that contains version information about this  object.
	 */
	var Version(default, never):cs.system.Version;
	@:overload(function(frameworkName:String):Void {})
	@:overload(function(identifier:String, version:cs.system.Version):Void {})
	function new(identifier:String, version:cs.system.Version, profile:String):Void;
	/**
	 * Returns a value that indicates whether two  objects represent the same .NET
	 * Framework version.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  and  parameters represent the same .NET Framework version;
	 * otherwise, .
	 */
	static function op_Equality(left:cs.system.runtime.versioning.FrameworkName, right:cs.system.runtime.versioning.FrameworkName):Bool;
	/**
	 * Returns a value that indicates whether two  objects represent different .NET
	 * Framework versions.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if the  and  parameters represent different .NET Framework versions;
	 * otherwise, .
	 */
	static function op_Inequality(left:cs.system.runtime.versioning.FrameworkName, right:cs.system.runtime.versioning.FrameworkName):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether this  instance represents the same .NET
	 * Framework version as a specified object.
	 * @param obj The object to compare to the current instance.
	 * @return if every component of the current  object matches the corresponding
	 * component of ; otherwise, .
	 */
	function Equals(other:cs.system.runtime.versioning.FrameworkName):Bool;
	/**
	 * Returns the hash code for the  object.
	 * @return A 32-bit signed integer that represents the hash code of this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of this  object.
	 * @return A string that represents this  object.
	 */
	function ToString():String;
}
