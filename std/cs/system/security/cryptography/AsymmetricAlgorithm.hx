package cs.system.security.cryptography;

/** Represents the abstract base class from which all implementations of asymmetric algorithms must inherit. */
@:native("System.Security.Cryptography.AsymmetricAlgorithm")
extern class AsymmetricAlgorithm {
	/**
	 * When overridden in a derived class, gets the name of the key exchange algorithm.
	 * Otherwise, throws an .
	 * @return The name of the key exchange algorithm.
	 */
	var KeyExchangeAlgorithm(default, never):String;
	/**
	 * Gets or sets the size, in bits, of the key modulus used by the asymmetric
	 * algorithm.
	 * @return The size, in bits, of the key modulus used by the asymmetric algorithm.
	 */
	var KeySize(default, default):Int;
	/**
	 * Gets the key sizes that are supported by the asymmetric algorithm.
	 * @return An array that contains the key sizes supported by the asymmetric
	 * algorithm.
	 */
	var LegalKeySizes(default, never):cs.NativeArray<cs.system.security.cryptography.KeySizes>;
	/**
	 * When implemented in a derived class, gets the name of the signature algorithm.
	 * Otherwise, always throws a .
	 * @return The name of the signature algorithm.
	 */
	var SignatureAlgorithm(default, never):String;
	@:overload(function():cs.system.security.cryptography.AsymmetricAlgorithm {})
	/**
	 * Creates a default cryptographic object used to perform the asymmetric algorithm.
	 * @return A new  instance, unless the default settings have been changed with the
	 * <cryptoClass> element.
	 */
	static function Create(algName:String):cs.system.security.cryptography.AsymmetricAlgorithm;
	/** Releases all resources used by the  class. */
	function Clear():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(passwordBytes:cs.system.ReadOnlySpan<cs.UInt8>, pbeParameters:cs.system.security.cryptography.PbeParameters):cs.NativeArray<cs.UInt8> {})
	function ExportEncryptedPkcs8PrivateKey(password:cs.system.ReadOnlySpan<cs.Char16>, pbeParameters:cs.system.security.cryptography.PbeParameters):cs.NativeArray<cs.UInt8>;
	function ExportPkcs8PrivateKey():cs.NativeArray<cs.UInt8>;
	function ExportSubjectPublicKeyInfo():cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, reconstructs an  object from an XML string.
	 * Otherwise, throws a .
	 * @param xmlString The XML string to use to reconstruct the  object.
	 */
	function FromXmlString(xmlString:String):Void;
	@:overload(function(passwordBytes:cs.system.ReadOnlySpan<cs.UInt8>, source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void {})
	function ImportEncryptedPkcs8PrivateKey(password:cs.system.ReadOnlySpan<cs.Char16>, source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	function ImportPkcs8PrivateKey(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	function ImportSubjectPublicKeyInfo(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	/**
	 * When overridden in a derived class, creates and returns an XML string
	 * representation of the current  object. Otherwise, throws a .
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return An XML string encoding of the current  object.
	 */
	function ToXmlString(includePrivateParameters:Bool):String;
	@:overload(function(passwordBytes:cs.system.ReadOnlySpan<cs.UInt8>, pbeParameters:cs.system.security.cryptography.PbeParameters, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool {})
	function TryExportEncryptedPkcs8PrivateKey(password:cs.system.ReadOnlySpan<cs.Char16>, pbeParameters:cs.system.security.cryptography.PbeParameters, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	function TryExportPkcs8PrivateKey(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	function TryExportSubjectPublicKeyInfo(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
