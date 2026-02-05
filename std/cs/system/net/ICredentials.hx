package cs.system.net;

/** Provides the base authentication interface for retrieving credentials for Web client authentication. */
@:native("System.Net.ICredentials")
extern interface ICredentials {
	/**
	 * Returns a  object that is associated with the specified URI, and authentication
	 * type.
	 * @param uri The  that the client is providing authentication for.
	 * @param authType The type of authentication, as defined in the  property.
	 * @return The  that is associated with the specified URI and authentication type,
	 * or, if no credentials are available, .
	 */
	function GetCredential(uri:cs.system.Uri, authType:String):cs.system.net.NetworkCredential;
}
