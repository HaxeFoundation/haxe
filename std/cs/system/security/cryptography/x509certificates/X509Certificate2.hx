package cs.system.security.cryptography.x509certificates;

/** Represents an X.509 certificate. */
@:native("System.Security.Cryptography.X509Certificates.X509Certificate2")
extern class X509Certificate2 extends cs.system.security.cryptography.x509certificates.X509Certificate {
	/**
	 * Gets or sets a value indicating that an X.509 certificate is archived.
	 * @return if the certificate is archived,  if the certificate is not archived.
	 */
	var Archived(default, default):Bool;
	/**
	 * Gets a collection of  objects.
	 * @return An  object.
	 */
	var Extensions(default, never):cs.system.security.cryptography.x509certificates.X509ExtensionCollection;
	/**
	 * Gets or sets the associated alias for a certificate.
	 * @return The certificate's friendly name.
	 */
	var FriendlyName(default, default):String;
	/**
	 * Gets a value that indicates whether an  object contains a private key.
	 * @return if the  object contains a private key; otherwise, .
	 */
	var HasPrivateKey(default, never):Bool;
	/**
	 * Gets the distinguished name of the certificate issuer.
	 * @return An  object that contains the name of the certificate issuer.
	 */
	var IssuerName(default, never):cs.system.security.cryptography.x509certificates.X500DistinguishedName;
	/**
	 * Gets the date in local time after which a certificate is no longer valid.
	 * @return A  object that represents the expiration date for the certificate.
	 */
	var NotAfter(default, never):cs.system.DateTime;
	/**
	 * Gets the date in local time on which a certificate becomes valid.
	 * @return A  object that represents the effective date of the certificate.
	 */
	var NotBefore(default, never):cs.system.DateTime;
	/**
	 * Gets or sets the  object that represents the private key associated with a
	 * certificate.
	 * @return An  object, which is either an RSA or DSA cryptographic service
	 * provider.
	 */
	var PrivateKey(default, default):cs.system.security.cryptography.AsymmetricAlgorithm;
	/**
	 * Gets a  object associated with a certificate.
	 * @return A  object.
	 */
	var PublicKey(default, never):cs.system.security.cryptography.x509certificates.PublicKey;
	/**
	 * Gets the raw data of a certificate.
	 * @return The raw data of the certificate as a byte array.
	 */
	var RawData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the serial number of a certificate as a big-endian hexadecimal string.
	 * @return The serial number of the certificate as a big-endian hexadecimal string.
	 */
	var SerialNumber(default, never):String;
	/**
	 * Gets the algorithm used to create the signature of a certificate.
	 * @return The object identifier of the signature algorithm.
	 */
	var SignatureAlgorithm(default, never):cs.system.security.cryptography.Oid;
	/**
	 * Gets the subject distinguished name from a certificate.
	 * @return An  object that represents the name of the certificate subject.
	 */
	var SubjectName(default, never):cs.system.security.cryptography.x509certificates.X500DistinguishedName;
	/**
	 * Gets the thumbprint of a certificate.
	 * @return The thumbprint of the certificate.
	 */
	var Thumbprint(default, never):String;
	/**
	 * Gets the X.509 format version of a certificate.
	 * @return The certificate format.
	 */
	var Version(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(handle:cs.system.IntPtr):Void {})
	@:overload(function(certificate:cs.system.security.cryptography.x509certificates.X509Certificate):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString):Void {})
	@:overload(function(fileName:String, password:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	function new(fileName:String, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void;
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.x509certificates.X509ContentType {})
	/**
	 * Indicates the type of certificate contained in a byte array.
	 * @param rawData A byte array containing data from an X.509 certificate.
	 * @return An  object.
	 */
	static function GetCertContentType(fileName:String):cs.system.security.cryptography.x509certificates.X509ContentType;
	/**
	 * Gets the subject and issuer names from a certificate.
	 * @param nameType The  value for the subject.
	 * @param forIssuer to include the issuer name; otherwise, .
	 * @return The name of the certificate.
	 */
	function GetNameInfo(nameType:cs.system.security.cryptography.x509certificates.X509NameType, forIssuer:Bool):String;
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	/**
	 * Populates an  object with data from a byte array.
	 * @param rawData A byte array containing data from an X.509 certificate.
	 */
	function Import(fileName:String, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void;
	/** Resets the state of an  object. */
	function Reset():Void;
	@:overload(function():String {})
	/**
	 * Displays an X.509 certificate in text format.
	 * @return The certificate information.
	 */
	function ToString(verbose:Bool):String;
	/**
	 * Performs a X.509 chain validation using basic validation policy.
	 * @return if the validation succeeds;  if the validation fails.
	 */
	function Verify():Bool;
}
