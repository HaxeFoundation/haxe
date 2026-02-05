package cs.system.net;

/**
 * Selects the authentication scheme for an  instance.
 * @param httpRequest The  instance for which to select an authentication scheme.
 * @return One of the  values that indicates the method of authentication to use
 * for the specified client request.
 */
@:native("System.Net.AuthenticationSchemeSelector")
extern class AuthenticationSchemeSelector extends cs.system.MulticastDelegate {
	function new(func:(httpRequest:cs.system.net.HttpListenerRequest)->cs.system.net.AuthenticationSchemes):Void;
	function Invoke(httpRequest:cs.system.net.HttpListenerRequest):cs.system.net.AuthenticationSchemes;
}
