package cs.system.security.cryptography.x509certificates;

/** Provides methods that help you use X.509 v.3 certificates. */
@:native("System.Security.Cryptography.X509Certificates.X509Certificate")
extern class X509Certificate {
	/**
	 * Gets a handle to a Microsoft Cryptographic API certificate context described by
	 * an unmanaged  structure.
	 * @return An  structure that represents an unmanaged  structure.
	 */
	var Handle(default, never):cs.system.IntPtr;
	/**
	 * Gets the name of the certificate authority that issued the X.509v3 certificate.
	 * @return The name of the certificate authority that issued the X.509v3
	 * certificate.
	 */
	var Issuer(default, never):String;
	/**
	 * Gets the subject distinguished name from the certificate.
	 * @return The subject distinguished name from the certificate.
	 */
	var Subject(default, never):String;
	@:overload(function():Void {})
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(handle:cs.system.IntPtr):Void {})
	@:overload(function(cert:cs.system.security.cryptography.x509certificates.X509Certificate):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String):Void {})
	@:overload(function(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString):Void {})
	@:overload(function(fileName:String, password:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	function new(fileName:String, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void;
	/**
	 * Creates an X.509v3 certificate from the specified PKCS7 signed file.
	 * @param filename The path of the PKCS7 signed file from which to create the X.509
	 * certificate.
	 * @return The newly created X.509 certificate.
	 */
	static function CreateFromCertFile(filename:String):cs.system.security.cryptography.x509certificates.X509Certificate;
	/**
	 * Creates an X.509v3 certificate from the specified signed file.
	 * @param filename The path of the signed file from which to create the X.509
	 * certificate.
	 * @return The newly created X.509 certificate.
	 */
	static function CreateFromSignedFile(filename:String):cs.system.security.cryptography.x509certificates.X509Certificate;
	/** Releases all resources used by the current  object. */
	function Dispose():Void;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Compares two  objects for equality.
	 * @param obj An  object to compare to the current object.
	 * @return if the current  object is equal to the object specified by the 
	 * parameter; otherwise, .
	 */
	function Equals(other:cs.system.security.cryptography.x509certificates.X509Certificate):Bool;
	@:overload(function(contentType:cs.system.security.cryptography.x509certificates.X509ContentType):cs.NativeArray<cs.UInt8> {})
	@:overload(function(contentType:cs.system.security.cryptography.x509certificates.X509ContentType, password:cs.system.security.SecureString):cs.NativeArray<cs.UInt8> {})
	/**
	 * Exports the current  object to a byte array in a format described by one of the 
	 * values.
	 * @param contentType One of the  values that describes how to format the output
	 * data.
	 * @return An array of bytes that represents the current  object.
	 */
	function Export(contentType:cs.system.security.cryptography.x509certificates.X509ContentType, password:String):cs.NativeArray<cs.UInt8>;
	@:overload(function():cs.NativeArray<cs.UInt8> {})
	/**
	 * Returns the hash value for the X.509v3 certificate as an array of bytes.
	 * @return The hash value for the X.509 certificate.
	 */
	function GetCertHash(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8>;
	@:overload(function():String {})
	/**
	 * Returns the SHA1 hash value for the X.509v3 certificate as a hexadecimal string.
	 * @return The hexadecimal string representation of the X.509 certificate hash
	 * value.
	 */
	function GetCertHashString(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):String;
	/**
	 * Returns the effective date of this X.509v3 certificate.
	 * @return The effective date for this X.509 certificate.
	 */
	function GetEffectiveDateString():String;
	/**
	 * Returns the expiration date of this X.509v3 certificate.
	 * @return The expiration date for this X.509 certificate.
	 */
	function GetExpirationDateString():String;
	/**
	 * Returns the name of the format of this X.509v3 certificate.
	 * @return The format of this X.509 certificate.
	 */
	function GetFormat():String;
	/**
	 * Returns the hash code for the X.509v3 certificate as an integer.
	 * @return The hash code for the X.509 certificate as an integer.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the name of the certification authority that issued the X.509v3
	 * certificate.
	 * @return The name of the certification authority that issued the X.509
	 * certificate.
	 */
	function GetIssuerName():String;
	/**
	 * Returns the key algorithm information for this X.509v3 certificate as a string.
	 * @return The key algorithm information for this X.509 certificate as a string.
	 */
	function GetKeyAlgorithm():String;
	/**
	 * Returns the key algorithm parameters for the X.509v3 certificate as an array of
	 * bytes.
	 * @return The key algorithm parameters for the X.509 certificate as an array of
	 * bytes.
	 */
	function GetKeyAlgorithmParameters():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the key algorithm parameters for the X.509v3 certificate as a
	 * hexadecimal string.
	 * @return The key algorithm parameters for the X.509 certificate as a hexadecimal
	 * string.
	 */
	function GetKeyAlgorithmParametersString():String;
	/**
	 * Returns the name of the principal to which the certificate was issued.
	 * @return The name of the principal to which the certificate was issued.
	 */
	function GetName():String;
	/**
	 * Returns the public key for the X.509v3 certificate as an array of bytes.
	 * @return The public key for the X.509 certificate as an array of bytes.
	 */
	function GetPublicKey():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the public key for the X.509v3 certificate as a hexadecimal string.
	 * @return The public key for the X.509 certificate as a hexadecimal string.
	 */
	function GetPublicKeyString():String;
	/**
	 * Returns the raw data for the entire X.509v3 certificate as an array of bytes.
	 * @return A byte array containing the X.509 certificate data.
	 */
	function GetRawCertData():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the raw data for the entire X.509v3 certificate as a hexadecimal string.
	 * @return The X.509 certificate data as a hexadecimal string.
	 */
	function GetRawCertDataString():String;
	/**
	 * Returns the serial number of the X.509v3 certificate as an array of bytes in
	 * little-endian order.
	 * @return The serial number of the X.509 certificate as an array of bytes in
	 * little-endian order.
	 */
	function GetSerialNumber():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the serial number of the X.509v3 certificate as a little-endian
	 * hexadecimal string .
	 * @return The serial number of the X.509 certificate as a little-endian
	 * hexadecimal string.
	 */
	function GetSerialNumberString():String;
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	@:overload(function(fileName:String, password:cs.system.security.SecureString, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	/**
	 * Populates the  object with data from a byte array.
	 * @param rawData A byte array containing data from an X.509 certificate.
	 */
	function Import(fileName:String, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void;
	/** Resets the state of the  object. */
	function Reset():Void;
	@:overload(function():String {})
	/**
	 * Returns a string representation of the current  object.
	 * @return A string representation of the current  object.
	 */
	function ToString(fVerbose:Bool):String;
	/**
	 * @param hashAlgorithm 
	 * @param destination 
	 * @param bytesWritten 
	 */
	function TryGetCertHash(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
