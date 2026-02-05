package cs.system.runtime.interopservices;

/** Represents an operating system platform. */
@:native("System.Runtime.InteropServices.OSPlatform")
extern class OSPlatform extends cs.system.ValueType {
	/**
	 * Gets an object that represents the Linux operating system.
	 * @return An object that represents the Linux operating system.
	 */
	static var Linux(default, never):cs.system.runtime.interopservices.OSPlatform;
	/**
	 * Gets an object that represents the OSX operating system.
	 * @return An object that represents the OSX operating system.
	 */
	static var OSX(default, never):cs.system.runtime.interopservices.OSPlatform;
	/**
	 * Gets an object that represents the Windows operating system.
	 * @return An object that represents the Windows operating system.
	 */
	static var Windows(default, never):cs.system.runtime.interopservices.OSPlatform;
	/**
	 * Creates a new  instance.
	 * @param osPlatform The name of the platform that this instance represents.
	 * @return An object that represents the  operating system.
	 */
	static function Create(osPlatform:String):cs.system.runtime.interopservices.OSPlatform;
	/**
	 * Determines whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.runtime.interopservices.OSPlatform, right:cs.system.runtime.interopservices.OSPlatform):Bool;
	/**
	 * Determines whether two  instances are unequal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are unequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.runtime.interopservices.OSPlatform, right:cs.system.runtime.interopservices.OSPlatform):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the current  instance is equal to the specified object.
	 * @param obj if  is a  instance and its name is the same as the current object;
	 * otherwise, false.
	 * @return if  is a  instance and its name is the same as the current object.
	 */
	function Equals(other:cs.system.runtime.interopservices.OSPlatform):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of this  instance.
	 * @return A string that represents this  instance.
	 */
	function ToString():String;
}
