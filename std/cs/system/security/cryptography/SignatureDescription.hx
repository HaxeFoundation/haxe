package cs.system.security.cryptography;

/** Contains information about the properties of a digital signature. */
@:native("System.Security.Cryptography.SignatureDescription")
extern class SignatureDescription {
	/**
	 * Gets or sets the deformatter algorithm for the signature description.
	 * @return The deformatter algorithm for the signature description.
	 */
	var DeformatterAlgorithm(default, default):String;
	/**
	 * Gets or sets the digest algorithm for the signature description.
	 * @return The digest algorithm for the signature description.
	 */
	var DigestAlgorithm(default, default):String;
	/**
	 * Gets or sets the formatter algorithm for the signature description.
	 * @return The formatter algorithm for the signature description.
	 */
	var FormatterAlgorithm(default, default):String;
	/**
	 * Gets or sets the key algorithm for the signature description.
	 * @return The key algorithm for the signature description.
	 */
	var KeyAlgorithm(default, default):String;
	@:overload(function():Void {})
	function new(el:cs.system.security.SecurityElement):Void;
	/**
	 * Creates an  instance with the specified key using the  property.
	 * @param key The key to use in the .
	 * @return The newly created  instance.
	 */
	function CreateDeformatter(key:cs.system.security.cryptography.AsymmetricAlgorithm):cs.system.security.cryptography.AsymmetricSignatureDeformatter;
	/**
	 * Creates a  instance using the  property.
	 * @return The newly created  instance.
	 */
	function CreateDigest():cs.system.security.cryptography.HashAlgorithm;
	/**
	 * Creates an  instance with the specified key using the  property.
	 * @param key The key to use in the .
	 * @return The newly created  instance.
	 */
	function CreateFormatter(key:cs.system.security.cryptography.AsymmetricAlgorithm):cs.system.security.cryptography.AsymmetricSignatureFormatter;
}
