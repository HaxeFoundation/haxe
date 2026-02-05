package cs.system.security.cryptography;

/** Provides an abstract base class from which all  implementations must inherit. */
@:native("System.Security.Cryptography.ECDiffieHellmanPublicKey")
extern class ECDiffieHellmanPublicKey {
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * When overridden in a derived class, exports the explicit  for an  object.
	 * @return An object that represents the point on the curve for this key, using the
	 * explicit curve format.
	 */
	function ExportExplicitParameters():cs.system.security.cryptography.ECParameters;
	/**
	 * When overridden in a derived class, exports the named or explicit  for an 
	 * object.
	 * @return An object that represents the point on the curve for this key.
	 */
	function ExportParameters():cs.system.security.cryptography.ECParameters;
	/**
	 * Serializes the  key BLOB to a byte array.
	 * @return A byte array that contains the serialized Elliptic Curve Diffie-Hellman
	 * (ECDH) public key.
	 */
	function ToByteArray():cs.NativeArray<cs.UInt8>;
	/**
	 * Serializes the  public key to an XML string.
	 * @return An XML string that contains the serialized Elliptic Curve Diffie-Hellman
	 * (ECDH) public key.
	 */
	function ToXmlString():String;
}
