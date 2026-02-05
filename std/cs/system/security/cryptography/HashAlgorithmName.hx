package cs.system.security.cryptography;

/** Specifies the name of a cryptographic hash algorithm. */
@:native("System.Security.Cryptography.HashAlgorithmName")
extern class HashAlgorithmName extends cs.system.ValueType {
	/**
	 * Gets a hash algorithm name that represents "MD5".
	 * @return A hash algorithm name that represents "MD5".
	 */
	static var MD5(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets a hash algorithm name that represents "SHA1".
	 * @return A hash algorithm name that represents "SHA1".
	 */
	static var SHA1(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets a hash algorithm name that represents "SHA256".
	 * @return A hash algorithm name that represents "SHA256".
	 */
	static var SHA256(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets a hash algorithm name that represents "SHA384".
	 * @return A hash algorithm name that represents "SHA384".
	 */
	static var SHA384(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets a hash algorithm name that represents "SHA512".
	 * @return A hash algorithm name that represents "SHA512".
	 */
	static var SHA512(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets the underlying string representation of the algorithm name.
	 * @return The string representation of the algorithm name, or  or  if no hash
	 * algorithm is available.
	 */
	var Name(default, never):String;
	function new(name:String):Void;
	/**
	 * Determines whether two specified  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if both  and  have the same  value; otherwise, .
	 */
	static function op_Equality(left:cs.system.security.cryptography.HashAlgorithmName, right:cs.system.security.cryptography.HashAlgorithmName):Bool;
	/**
	 * Determines whether two specified  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if both  and  do not have the same  value; otherwise, .
	 */
	static function op_Inequality(left:cs.system.security.cryptography.HashAlgorithmName, right:cs.system.security.cryptography.HashAlgorithmName):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current instance and a specified
	 * object are equal.
	 * @param obj The object to compare with the current instance.
	 * @return if  is a  object and its  property is equal to that of the current
	 * instance. The comparison is ordinal and case-sensitive.
	 */
	function Equals(other:cs.system.security.cryptography.HashAlgorithmName):Bool;
	/**
	 * Returns the hash code for the current instance.
	 * @return The hash code for the current instance, or 0 if no  value was supplied
	 * to the  constructor.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of the current  instance.
	 * @return The string representation of the current  instance.
	 */
	function ToString():String;
}
