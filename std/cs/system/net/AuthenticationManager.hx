package cs.system.net;

/** Manages the authentication modules called during the client authentication process. */
@:native("System.Net.AuthenticationManager")
extern class AuthenticationManager {
	/**
	 * Gets or sets the credential policy to be used for resource requests made using
	 * the  class.
	 * @return An object that implements the  interface that determines whether
	 * credentials are sent with requests. The default value is .
	 */
	static var CredentialPolicy(default, default):cs.system.net.ICredentialPolicy;
	/**
	 * Gets the dictionary that contains Service Principal Names (SPNs) that are used
	 * to identify hosts during Kerberos authentication for requests made using  and
	 * its derived classes.
	 * @return A writable  that contains the SPN values for keys composed of host
	 * information.
	 */
	static var CustomTargetNameDictionary(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets a list of authentication modules that are registered with the
	 * authentication manager.
	 * @return An  that enables the registered authentication modules to be read.
	 */
	static var RegisteredModules(default, never):cs.system.collections.IEnumerator;
	/**
	 * Calls each registered authentication module to find the first module that can
	 * respond to the authentication request.
	 * @param challenge The challenge returned by the Internet resource.
	 * @param request The  that initiated the authentication challenge.
	 * @param credentials The  associated with this request.
	 * @return An instance of the  class containing the result of the authorization
	 * attempt. If there is no authentication module to respond to the challenge, this
	 * method returns .
	 */
	static function Authenticate(challenge:String, request:cs.system.net.WebRequest, credentials:cs.system.net.ICredentials):cs.system.net.Authorization;
	/**
	 * Preauthenticates a request.
	 * @param request A  to an Internet resource.
	 * @param credentials The  associated with the request.
	 * @return An instance of the  class if the request can be preauthenticated;
	 * otherwise, . If  is , this method returns .
	 */
	static function PreAuthenticate(request:cs.system.net.WebRequest, credentials:cs.system.net.ICredentials):cs.system.net.Authorization;
	/**
	 * Registers an authentication module with the authentication manager.
	 * @param authenticationModule The  to register with the authentication manager.
	 */
	static function Register(authenticationModule:cs.system.net.IAuthenticationModule):Void;
	@:overload(function(authenticationModule:cs.system.net.IAuthenticationModule):Void {})
	/**
	 * Removes the specified authentication module from the list of registered modules.
	 * @param authenticationModule The  to remove from the list of registered modules.
	 */
	static function Unregister(authenticationScheme:String):Void;
}
