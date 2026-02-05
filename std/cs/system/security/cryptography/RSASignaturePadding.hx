package cs.system.security.cryptography;

/** Specifies the padding mode and parameters to use with RSA signature creation or verification operations. */
@:native("System.Security.Cryptography.RSASignaturePadding")
extern class RSASignaturePadding {
	/**
	 * Gets an object that uses the PKCS #1 v1.5 padding mode.
	 * @return An object that uses the  padding mode.
	 */
	static var Pkcs1(default, never):cs.system.security.cryptography.RSASignaturePadding;
	/**
	 * Gets an object that uses PSS padding mode.
	 * @return An object that uses the  padding mode with the number of salt bytes
	 * equal to the size of the hash.
	 */
	static var Pss(default, never):cs.system.security.cryptography.RSASignaturePadding;
	/**
	 * Gets the padding mode of this  instance.
	 * @return The padding mode (either  or ) of this instance.
	 */
	var Mode(default, never):cs.system.security.cryptography.RSASignaturePaddingMode;
	/**
	 * Indicates whether two specified  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.security.cryptography.RSASignaturePadding, right:cs.system.security.cryptography.RSASignaturePadding):Bool;
	/**
	 * Indicates whether two specified  objects are unequal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are unequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.security.cryptography.RSASignaturePadding, right:cs.system.security.cryptography.RSASignaturePadding):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj The object to compare with the current instance.
	 * @return if the specified object is equal to the current object; otherwise, .
	 */
	function Equals(other:cs.system.security.cryptography.RSASignaturePadding):Bool;
	/**
	 * Returns the hash code for this  instance.
	 * @return The hash code for this  instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of the current  instance.
	 * @return The string representation of the current object.
	 */
	function ToString():String;
}
