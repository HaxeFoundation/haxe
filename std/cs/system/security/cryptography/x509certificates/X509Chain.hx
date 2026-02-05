package cs.system.security.cryptography.x509certificates;

/** Represents a chain-building engine for  certificates. */
@:native("System.Security.Cryptography.X509Certificates.X509Chain")
extern class X509Chain {
	/**
	 * Gets a handle to an X.509 chain.
	 * @return An  handle to an X.509 chain.
	 */
	var ChainContext(default, never):cs.system.IntPtr;
	/**
	 * Gets a collection of  objects.
	 * @return An  object.
	 */
	var ChainElements(default, never):cs.system.security.cryptography.x509certificates.X509ChainElementCollection;
	/**
	 * Gets or sets the  to use when building an X.509 certificate chain.
	 * @return The  object associated with this X.509 chain.
	 */
	var ChainPolicy(default, default):cs.system.security.cryptography.x509certificates.X509ChainPolicy;
	/**
	 * Gets the status of each element in an  object.
	 * @return An array of  objects.
	 */
	var ChainStatus(default, never):cs.NativeArray<cs.system.security.cryptography.x509certificates.X509ChainStatus>;
	/**
	 * Gets a safe handle for this  instance.
	 * @return The safe handle for this  instance.
	 */
	var SafeHandle(default, never):cs.microsoft.win32.safehandles.SafeX509ChainHandle;
	@:overload(function():Void {})
	@:overload(function(useMachineContext:Bool):Void {})
	function new(chainContext:cs.system.IntPtr):Void;
	/**
	 * Creates an  object after querying for the mapping defined in the CryptoConfig
	 * file, and maps the chain to that mapping.
	 * @return An  object.
	 */
	static function Create():cs.system.security.cryptography.x509certificates.X509Chain;
	/**
	 * Builds an X.509 chain using the policy specified in .
	 * @param certificate An  object.
	 * @return if the X.509 certificate is valid; otherwise, .
	 */
	function Build(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Bool;
	/** Releases all of the resources used by this . */
	function Dispose():Void;
	/** Clears the current  object. */
	function Reset():Void;
}
