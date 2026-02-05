package cs.system.security.cryptography;

/** Provides the ability to navigate through an  object. This class cannot be inherited. */
@:native("System.Security.Cryptography.AsnEncodedDataEnumerator")
extern class AsnEncodedDataEnumerator {
	/**
	 * Gets the current  object in an  object.
	 * @return The current  object in the collection.
	 */
	var Current(default, never):cs.system.security.cryptography.AsnEncodedData;
	/**
	 * Advances to the next  object in an  object.
	 * @return , if the enumerator was successfully advanced to the next element; , if
	 * the enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets an enumerator to its initial position. */
	function Reset():Void;
}
