package cs.system.security.cryptography.x509certificates;

/** Supports a simple iteration over an . This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainElementEnumerator")
extern class X509ChainElementEnumerator {
	/**
	 * Gets the current element in the .
	 * @return The current element in the .
	 */
	var Current(default, never):cs.system.security.cryptography.x509certificates.X509ChainElement;
	/**
	 * Advances the enumerator to the next element in the .
	 * @return if the enumerator was successfully advanced to the next element;  if the
	 * enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first element in the . */
	function Reset():Void;
}
