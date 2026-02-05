package cs.system;

/** Represents the version number of an assembly, operating system, or the common language runtime. This class cannot be inherited. */
@:native("System.Version")
extern class Version {
	/**
	 * Gets the value of the build component of the version number for the current 
	 * object.
	 * @return The build number, or -1 if the build number is undefined.
	 */
	var Build(default, never):Int;
	/**
	 * Gets the value of the major component of the version number for the current 
	 * object.
	 * @return The major version number.
	 */
	var Major(default, never):Int;
	/**
	 * Gets the high 16 bits of the revision number.
	 * @return A 16-bit signed integer.
	 */
	var MajorRevision(default, never):cs.Int16;
	/**
	 * Gets the value of the minor component of the version number for the current 
	 * object.
	 * @return The minor version number.
	 */
	var Minor(default, never):Int;
	/**
	 * Gets the low 16 bits of the revision number.
	 * @return A 16-bit signed integer.
	 */
	var MinorRevision(default, never):cs.Int16;
	/**
	 * Gets the value of the revision component of the version number for the current 
	 * object.
	 * @return The revision number, or -1 if the revision number is undefined.
	 */
	var Revision(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(version:String):Void {})
	@:overload(function(major:Int, minor:Int):Void {})
	@:overload(function(major:Int, minor:Int, build:Int):Void {})
	function new(major:Int, minor:Int, build:Int, revision:Int):Void;
	/**
	 * Determines whether two specified  objects are equal.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  equals ; otherwise, .
	 */
	static function op_Equality(v1:cs.system.Version, v2:cs.system.Version):Bool;
	/**
	 * Determines whether the first specified  object is greater than the second
	 * specified  object.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  is greater than ; otherwise, .
	 */
	static function op_GreaterThan(v1:cs.system.Version, v2:cs.system.Version):Bool;
	/**
	 * Determines whether the first specified  object is greater than or equal to the
	 * second specified  object.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  is greater than or equal to ; otherwise, .
	 */
	static function op_GreaterThanOrEqual(v1:cs.system.Version, v2:cs.system.Version):Bool;
	/**
	 * Determines whether two specified  objects are not equal.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  does not equal ; otherwise, .
	 */
	static function op_Inequality(v1:cs.system.Version, v2:cs.system.Version):Bool;
	/**
	 * Determines whether the first specified  object is less than the second specified
	 * object.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  is less than ; otherwise, .
	 */
	static function op_LessThan(v1:cs.system.Version, v2:cs.system.Version):Bool;
	/**
	 * Determines whether the first specified  object is less than or equal to the
	 * second  object.
	 * @param v1 The first  object.
	 * @param v2 The second  object.
	 * @return if  is less than or equal to ; otherwise, .
	 */
	static function op_LessThanOrEqual(v1:cs.system.Version, v2:cs.system.Version):Bool;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>):cs.system.Version {})
	/** @param input  */
	static function Parse(input:String):cs.system.Version;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.Version>):Bool {})
	/**
	 * @param input 
	 * @param result 
	 */
	static function TryParse(input:String, result:cs.Ref<cs.system.Version>):Bool;
	/**
	 * Returns a new  object whose value is the same as the current  object.
	 * @return A new  whose values are a copy of the current  object.
	 */
	function Clone():Dynamic;
	@:overload(function(version:Dynamic):Int {})
	/**
	 * Compares the current  object to a specified object and returns an indication of
	 * their relative values.
	 * @param version An object to compare, or .
	 * @return A signed integer that indicates the relative values of the two objects,
	 * as shown in the following table. Return value Meaning Less than zero The current
	 * object is a version before . Zero The current  object is the same version as .
	 * Greater than zero The current  object is a version subsequent to . -or- is .
	 */
	function CompareTo(value:cs.system.Version):Int;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value indicating whether the current  object is equal to a specified
	 * object.
	 * @param obj An object to compare with the current  object, or .
	 * @return if the current  object and  are both  objects, and every component of
	 * the current  object matches the corresponding component of ; otherwise, .
	 */
	function Equals(obj:cs.system.Version):Bool;
	/**
	 * Returns a hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function():String {})
	/**
	 * Converts the value of the current  object to its equivalent  representation.
	 * @return The  representation of the values of the major, minor, build, and
	 * revision components of the current  object, as depicted in the following format.
	 * Each component is separated by a period character ('.'). Square brackets ('['
	 * and ']') indicate a component that will not appear in the return value if the
	 * component is not defined: major.minor[.build[.revision]] For example, if you
	 * create a  object using the constructor Version(1,1), the returned string is
	 * "1.1". If you create a  object using the constructor Version(1,3,4,2), the
	 * returned string is "1.3.4.2".
	 */
	function ToString(fieldCount:Int):String;
	@:overload(function(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>):Bool {})
	/**
	 * @param destination 
	 * @param fieldCount 
	 * @param charsWritten 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, fieldCount:Int, charsWritten:cs.Ref<Int>):Bool;
}
