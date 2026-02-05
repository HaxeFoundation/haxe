package cs.system.security.cryptography.x509certificates;

/** This class facilitates building a subject alternative name extension for an X.509 certificate. */
@:native("System.Security.Cryptography.X509Certificates.SubjectAlternativeNameBuilder")
extern class SubjectAlternativeNameBuilder {
	function new():Void;
	/**
	 * Adds a DNS Name to the subject alternative name extension.
	 * @param dnsName The DNS name to be added.
	 */
	function AddDnsName(dnsName:String):Void;
	/**
	 * Adds an email address to the subject alternative name extension.
	 * @param emailAddress The email address to be added.
	 */
	function AddEmailAddress(emailAddress:String):Void;
	/**
	 * Adds an IP address to the subject alternative name extension.
	 * @param ipAddress The IP address to be added.
	 */
	function AddIpAddress(ipAddress:cs.system.net.IPAddress):Void;
	/**
	 * Adds a Uniform Resource Identifier (URI) to the subject alternative name
	 * extension.
	 * @param uri The URI to be added.
	 */
	function AddUri(uri:cs.system.Uri):Void;
	/**
	 * Adds a User Principal Name (UPN) to the subject alternative name extension.
	 * @param upn The UPN to be added.
	 */
	function AddUserPrincipalName(upn:String):Void;
	/**
	 * Returns an  object that represents the encoded subject alternative name
	 * extension.
	 * @param critical to mark the extension as ; otherwise, . The default value is .
	 * @return An  object that represents the encoded subject alternative name
	 * extension.
	 */
	function Build(?critical:Bool):cs.system.security.cryptography.x509certificates.X509Extension;
}
