package cs.system.security;

/** Defines the methods that convert permission object state to and from XML element representation. */
@:native("System.Security.ISecurityEncodable")
extern interface ISecurityEncodable {
	/**
	 * Reconstructs a security object with a specified state from an XML encoding.
	 * @param e The XML encoding to use to reconstruct the security object.
	 */
	function FromXml(e:cs.system.security.SecurityElement):Void;
	/**
	 * Creates an XML encoding of the security object and its current state.
	 * @return An XML encoding of the security object, including any state information.
	 */
	function ToXml():cs.system.security.SecurityElement;
}
