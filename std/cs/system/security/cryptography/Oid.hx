package cs.system.security.cryptography;

/** Represents a cryptographic object identifier. This class cannot be inherited. */
@:native("System.Security.Cryptography.Oid")
extern class Oid {
	/**
	 * Gets or sets the friendly name of the identifier.
	 * @return The friendly name of the identifier.
	 */
	var FriendlyName(default, default):String;
	/**
	 * Gets or sets the dotted number of the identifier.
	 * @return The dotted number of the identifier.
	 */
	var Value(default, default):String;
	@:overload(function():Void {})
	@:overload(function(oid:cs.system.security.cryptography.Oid):Void {})
	@:overload(function(oid:String):Void {})
	function new(value:String, friendlyName:String):Void;
	/**
	 * Creates an  object from an OID friendly name by searching the specified group.
	 * @param friendlyName The friendly name of the identifier.
	 * @param group The group to search in.
	 * @return An object that represents the specified OID.
	 */
	static function FromFriendlyName(friendlyName:String, group:cs.system.security.cryptography.OidGroup):cs.system.security.cryptography.Oid;
	/**
	 * Creates an  object by using the specified OID value and group.
	 * @param oidValue The OID value.
	 * @param group The group to search in.
	 * @return A new instance of an  object.
	 */
	static function FromOidValue(oidValue:String, group:cs.system.security.cryptography.OidGroup):cs.system.security.cryptography.Oid;
}
