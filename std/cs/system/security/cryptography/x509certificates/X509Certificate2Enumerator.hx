package cs.system.security.cryptography.x509certificates;

/** Supports a simple iteration over a  object. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509Certificate2Enumerator")
extern class X509Certificate2Enumerator {
	/**
	 * Gets the current element in the  object.
	 * @return The current element in the  object.
	 */
	var Current(default, never):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Advances the enumerator to the next element in the  object.
	 * @return if the enumerator was successfully advanced to the next element;  if the
	 * enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first element in the  object. */
	function Reset():Void;
}
