package cs.system.security.cryptography.x509certificates;

/** Provides a simple structure for storing X509 chain status and error information. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainStatus")
extern class X509ChainStatus extends cs.system.ValueType {
	/**
	 * Specifies the status of the X509 chain.
	 * @return An  value.
	 */
	var Status(default, default):cs.system.security.cryptography.x509certificates.X509ChainStatusFlags;
	/**
	 * Specifies a description of the  value.
	 * @return A localizable string.
	 */
	var StatusInformation(default, default):String;
}
