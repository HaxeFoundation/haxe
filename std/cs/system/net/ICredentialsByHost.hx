package cs.system.net;

/** Provides the interface for retrieving credentials for a host, port, and authentication type. */
@:native("System.Net.ICredentialsByHost")
extern interface ICredentialsByHost {
	/**
	 * Returns the credential for the specified host, port, and authentication
	 * protocol.
	 * @param host The host computer that is authenticating the client.
	 * @param port The port on  that the client will communicate with.
	 * @param authenticationType The authentication protocol.
	 * @return A  for the specified host, port, and authentication protocol, or  if
	 * there are no credentials available for the specified host, port, and
	 * authentication protocol.
	 */
	function GetCredential(host:String, port:Int, authenticationType:String):cs.system.net.NetworkCredential;
}
