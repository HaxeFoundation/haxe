package cs.system.net;

/** Defines the credential policy to be used for resource requests that are made using  and its derived classes. */
@:native("System.Net.ICredentialPolicy")
extern interface ICredentialPolicy {
	/**
	 * Returns a  that indicates whether the client's credentials are sent with a
	 * resource request made using an instance of the  class.
	 * @param challengeUri The  that will receive the request.
	 * @param request The  that represents the resource being requested.
	 * @param credential The  that will be sent with the request if this method returns
	 * .
	 * @param authenticationModule The  that will conduct the authentication, if
	 * authentication is required.
	 * @return if the credentials are sent with the request; otherwise, .
	 */
	function ShouldSendCredential(challengeUri:cs.system.Uri, request:cs.system.net.WebRequest, credential:cs.system.net.NetworkCredential, authenticationModule:cs.system.net.IAuthenticationModule):Bool;
}
