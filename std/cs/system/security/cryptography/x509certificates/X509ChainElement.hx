package cs.system.security.cryptography.x509certificates;

/** Represents an element of an X.509 chain. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainElement")
extern class X509ChainElement {
	/**
	 * Gets the X.509 certificate at a particular chain element.
	 * @return An  object.
	 */
	var Certificate(default, never):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Gets the error status of the current X.509 certificate in a chain.
	 * @return An array of  objects.
	 */
	var ChainElementStatus(default, never):cs.NativeArray<cs.system.security.cryptography.x509certificates.X509ChainStatus>;
	/**
	 * Gets additional error information from an unmanaged certificate chain structure.
	 * @return A string representing the  member of the unmanaged  structure in the
	 * Crypto API.
	 */
	var Information(default, never):String;
}
