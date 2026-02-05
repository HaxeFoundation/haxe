package cs.system.security.cryptography.x509certificates;

/** Represents the distinguished name of an X509 certificate. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X500DistinguishedName")
extern class X500DistinguishedName extends cs.system.security.cryptography.AsnEncodedData {
	/**
	 * Gets the comma-delimited distinguished name from an X500 certificate.
	 * @return The comma-delimited distinguished name of the X509 certificate.
	 */
	var Name(default, never):String;
	@:overload(function(encodedDistinguishedName:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(encodedDistinguishedName:cs.system.security.cryptography.AsnEncodedData):Void {})
	@:overload(function(distinguishedName:cs.system.security.cryptography.x509certificates.X500DistinguishedName):Void {})
	@:overload(function(distinguishedName:String):Void {})
	function new(distinguishedName:String, flag:cs.system.security.cryptography.x509certificates.X500DistinguishedNameFlags):Void;
	/**
	 * Decodes a distinguished name using the characteristics specified by the 
	 * parameter.
	 * @param flag A bitwise combination of the enumeration values that specify the
	 * characteristics of the distinguished name.
	 * @return The decoded distinguished name.
	 */
	function Decode(flag:cs.system.security.cryptography.x509certificates.X500DistinguishedNameFlags):String;
	/**
	 * Returns a formatted version of an X500 distinguished name for printing or for
	 * output to a text window or to a console.
	 * @param multiLine if the return string should contain carriage returns;
	 * otherwise, .
	 * @return A formatted string that represents the X500 distinguished name.
	 */
	function Format(multiLine:Bool):String;
}
