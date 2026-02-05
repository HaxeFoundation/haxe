package cs.system.security.cryptography.x509certificates;

/** Represents a certificate's public key information. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.PublicKey")
extern class PublicKey {
	/**
	 * Gets the ASN.1-encoded representation of the public key value.
	 * @return The ASN.1-encoded representation of the public key value.
	 */
	var EncodedKeyValue(default, never):cs.system.security.cryptography.AsnEncodedData;
	/**
	 * Gets the ASN.1-encoded representation of the public key parameters.
	 * @return The ASN.1-encoded representation of the public key parameters.
	 */
	var EncodedParameters(default, never):cs.system.security.cryptography.AsnEncodedData;
	/**
	 * Gets an  derived object or a  derived object representing the public key.
	 * @return An  object representing the public key.
	 */
	var Key(default, never):cs.system.security.cryptography.AsymmetricAlgorithm;
	/**
	 * Gets an object identifier (OID) object of the public key.
	 * @return An object identifier (OID) object of the public key.
	 */
	var Oid(default, never):cs.system.security.cryptography.Oid;
	function new(oid:cs.system.security.cryptography.Oid, parameters:cs.system.security.cryptography.AsnEncodedData, keyValue:cs.system.security.cryptography.AsnEncodedData):Void;
}
