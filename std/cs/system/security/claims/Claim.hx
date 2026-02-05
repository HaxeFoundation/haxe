package cs.system.security.claims;

/** Represents a claim. */
@:native("System.Security.Claims.Claim")
extern class Claim {
	/**
	 * Contains any additional data provided by a derived type.
	 * @return A  array representing the additional serialized data.
	 */
	var CustomSerializationData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the issuer of the claim.
	 * @return A name that refers to the issuer of the claim.
	 */
	var Issuer(default, never):String;
	/**
	 * Gets the original issuer of the claim.
	 * @return A name that refers to the original issuer of the claim.
	 */
	var OriginalIssuer(default, never):String;
	/**
	 * Gets a dictionary that contains additional properties associated with this
	 * claim.
	 * @return A dictionary that contains additional properties associated with the
	 * claim. The properties are represented as name-value pairs.
	 */
	var Properties(default, never):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets the subject of the claim.
	 * @return The subject of the claim.
	 */
	var Subject(default, never):cs.system.security.claims.ClaimsIdentity;
	/**
	 * Gets the claim type of the claim.
	 * @return The claim type.
	 */
	var Type(default, never):String;
	/**
	 * Gets the value of the claim.
	 * @return The claim value.
	 */
	var Value(default, never):String;
	/**
	 * Gets the value type of the claim.
	 * @return The claim value type.
	 */
	var ValueType(default, never):String;
	@:overload(function(reader:cs.system.io.BinaryReader):Void {})
	@:overload(function(reader:cs.system.io.BinaryReader, subject:cs.system.security.claims.ClaimsIdentity):Void {})
	@:overload(function(type:String, value:String):Void {})
	@:overload(function(type:String, value:String, valueType:String):Void {})
	@:overload(function(type:String, value:String, valueType:String, issuer:String):Void {})
	@:overload(function(type:String, value:String, valueType:String, issuer:String, originalIssuer:String):Void {})
	function new(type:String, value:String, valueType:String, issuer:String, originalIssuer:String, subject:cs.system.security.claims.ClaimsIdentity):Void;
	@:overload(function():cs.system.security.claims.Claim {})
	/**
	 * Returns a new  object copied from this object. The new claim does not have a
	 * subject.
	 * @return The new claim object.
	 */
	function Clone(identity:cs.system.security.claims.ClaimsIdentity):cs.system.security.claims.Claim;
	/**
	 * Returns a string representation of this  object.
	 * @return The string representation of this  object.
	 */
	function ToString():String;
	/**
	 * Writes this  to the writer.
	 * @param writer The writer to use for data storage.
	 */
	function WriteTo(writer:cs.system.io.BinaryWriter):Void;
}
