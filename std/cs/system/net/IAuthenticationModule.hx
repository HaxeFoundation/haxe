package cs.system.net;

/** Provides the base authentication interface for Web client authentication modules. */
@:native("System.Net.IAuthenticationModule")
extern interface IAuthenticationModule {
	/**
	 * Gets the authentication type provided by this authentication module.
	 * @return A string indicating the authentication type provided by this
	 * authentication module.
	 */
	var AuthenticationType(default, never):String;
	/**
	 * Gets a value indicating whether the authentication module supports
	 * preauthentication.
	 * @return if the authorization module supports preauthentication; otherwise .
	 */
	var CanPreAuthenticate(default, never):Bool;
	/**
	 * Returns an instance of the  class in response to an authentication challenge
	 * from a server.
	 * @param challenge The authentication challenge sent by the server.
	 * @param request The  instance associated with the challenge.
	 * @param credentials The credentials associated with the challenge.
	 * @return An  instance containing the authorization message for the request, or 
	 * if the challenge cannot be handled.
	 */
	function Authenticate(challenge:String, request:cs.system.net.WebRequest, credentials:cs.system.net.ICredentials):cs.system.net.Authorization;
	/**
	 * Returns an instance of the  class for an authentication request to a server.
	 * @param request The  instance associated with the authentication request.
	 * @param credentials The credentials associated with the authentication request.
	 * @return An  instance containing the authorization message for the request.
	 */
	function PreAuthenticate(request:cs.system.net.WebRequest, credentials:cs.system.net.ICredentials):cs.system.net.Authorization;
}
