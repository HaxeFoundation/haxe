package cs.system.net.http;

/** The default message handler used by  in .NET Framework and .NET Core 2.0 and earlier. */
@:native("System.Net.Http.HttpClientHandler")
extern class HttpClientHandler extends cs.system.net.http.HttpMessageHandler {
	/**
	 * Gets a cached delegate that always returns .
	 * @return A cached delegate that always returns .
	 */
	static var DangerousAcceptAnyServerCertificateValidator(default, never):cs.system.Func_5<cs.system.net.http.HttpRequestMessage, cs.system.security.cryptography.x509certificates.X509Certificate2, cs.system.security.cryptography.x509certificates.X509Chain, cs.system.net.security.SslPolicyErrors, Bool>;
	/**
	 * Gets or sets a value that indicates whether the handler should follow
	 * redirection responses.
	 * @return if the handler should follow redirection responses; otherwise . The
	 * default value is .
	 */
	var AllowAutoRedirect(default, default):Bool;
	/**
	 * Gets or sets the type of decompression method used by the handler for automatic
	 * decompression of the HTTP content response.
	 * @return The automatic decompression method used by the handler.
	 */
	var AutomaticDecompression(default, default):cs.system.net.DecompressionMethods;
	/**
	 * Gets or sets a value that indicates whether the certificate is checked against
	 * the certificate authority revocation list.
	 * @return if the certificate revocation list is checked; otherwise, .
	 */
	var CheckCertificateRevocationList(default, default):Bool;
	/**
	 * Gets or sets a value that indicates if the certificate is automatically picked
	 * from the certificate store or if the caller is allowed to pass in a specific
	 * client certificate.
	 * @return The collection of security certificates associated with this handler.
	 */
	var ClientCertificateOptions(default, default):cs.system.net.http.ClientCertificateOption;
	/**
	 * Gets the collection of security certificates that are associated requests to the
	 * server.
	 * @return The X509CertificateCollection that is presented to the server when
	 * performing certificate based client authentication.
	 */
	var ClientCertificates(default, never):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	/**
	 * Gets or sets the cookie container used to store server cookies by the handler.
	 * @return The cookie container used to store server cookies by the handler.
	 */
	var CookieContainer(default, default):cs.system.net.CookieContainer;
	/**
	 * Gets or sets authentication information used by this handler.
	 * @return The authentication credentials associated with the handler. The default
	 * is .
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * When the default (system) proxy is being used, gets or sets the credentials to
	 * submit to the default proxy server for authentication. The default proxy is used
	 * only when  is set to  and  is set to .
	 * @return The credentials needed to authenticate a request to the default proxy
	 * server.
	 */
	var DefaultProxyCredentials(default, default):cs.system.net.ICredentials;
	/**
	 * Gets or sets the maximum number of redirects that the handler follows.
	 * @return The maximum number of redirection responses that the handler follows.
	 * The default value is 50.
	 */
	var MaxAutomaticRedirections(default, default):Int;
	/**
	 * Gets or sets the maximum number of concurrent connections (per server endpoint)
	 * allowed when making requests using an  object. Note that the limit is per server
	 * endpoint, so for example a value of 256 would permit 256 concurrent connections
	 * to http://www.adatum.com/ and another 256 to http://www.adventure-works.com/.
	 * @return The maximum number of concurrent connections (per server endpoint)
	 * allowed by an  object.
	 */
	var MaxConnectionsPerServer(default, default):Int;
	/**
	 * Gets or sets the maximum request content buffer size used by the handler.
	 * @return The maximum request content buffer size in bytes. The default value is 2
	 * gigabytes.
	 */
	var MaxRequestContentBufferSize(default, default):haxe.Int64;
	/**
	 * Gets or sets the maximum length, in kilobytes (1024 bytes), of the response
	 * headers. For example, if the value is 64, then 65536 bytes are allowed for the
	 * maximum response headers' length.
	 * @return The maximum length, in kilobytes (1024 bytes), of the response headers.
	 */
	var MaxResponseHeadersLength(default, default):Int;
	/**
	 * Gets or sets a value that indicates whether the handler sends an Authorization
	 * header with the request.
	 * @return for the handler to send an HTTP Authorization header with requests after
	 * authentication has taken place; otherwise, . The default is .
	 */
	var PreAuthenticate(default, default):Bool;
	/**
	 * Gets a writable dictionary (that is, a map) of custom properties for the 
	 * requests. The dictionary is initialized empty; you can insert and query
	 * key-value pairs for your custom handlers and special processing.
	 * @return a writable dictionary of custom properties.
	 */
	var Properties(default, never):cs.system.collections.generic.IDictionary<String, Dynamic>;
	/**
	 * Gets or sets proxy information used by the handler.
	 * @return The proxy information used by the handler. The default value is .
	 */
	var Proxy(default, default):cs.system.net.IWebProxy;
	/**
	 * Gets or sets a callback method to validate the server certificate.
	 * @return A callback method to validate the server certificate.
	 */
	var ServerCertificateCustomValidationCallback(default, default):cs.system.Func_5<cs.system.net.http.HttpRequestMessage, cs.system.security.cryptography.x509certificates.X509Certificate2, cs.system.security.cryptography.x509certificates.X509Chain, cs.system.net.security.SslPolicyErrors, Bool>;
	/**
	 * Gets or sets the TLS/SSL protocol used by the  objects managed by the
	 * HttpClientHandler object.
	 * @return One of the values defined in the  enumeration.
	 */
	var SslProtocols(default, default):cs.system.security.authentication.SslProtocols;
	/**
	 * Gets a value that indicates whether the handler supports automatic response
	 * content decompression.
	 * @return if the if the handler supports automatic response content decompression;
	 * otherwise . The default value is .
	 */
	var SupportsAutomaticDecompression(default, never):Bool;
	/**
	 * Gets a value that indicates whether the handler supports proxy settings.
	 * @return if the if the handler supports proxy settings; otherwise . The default
	 * value is .
	 */
	var SupportsProxy(default, never):Bool;
	/**
	 * Gets a value that indicates whether the handler supports configuration settings
	 * for the  and  properties.
	 * @return if the if the handler supports configuration settings for the  and 
	 * properties; otherwise . The default value is .
	 */
	var SupportsRedirectConfiguration(default, never):Bool;
	/**
	 * Gets or sets a value that indicates whether the handler uses the   property  to
	 * store server cookies and uses these cookies when sending requests.
	 * @return if the if the handler supports uses the   property  to store server
	 * cookies and uses these cookies when sending requests; otherwise . The default
	 * value is .
	 */
	var UseCookies(default, default):Bool;
	/**
	 * Gets or sets a value that controls whether default credentials are sent with
	 * requests by the handler.
	 * @return if the default credentials are used; otherwise . The default value is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether the handler uses a proxy for
	 * requests.
	 * @return if the handler should use a proxy for requests; otherwise . The default
	 * value is .
	 */
	var UseProxy(default, default):Bool;
	function new():Void;
}
