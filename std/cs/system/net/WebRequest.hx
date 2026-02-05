package cs.system.net;

/** Makes a request to a Uniform Resource Identifier (URI). This is an  class. */
@:native("System.Net.WebRequest")
extern class WebRequest extends cs.system.MarshalByRefObject {
	/**
	 * Gets or sets the default cache policy for this request.
	 * @return A  that specifies the cache policy in effect for this request when no
	 * other policy is applicable.
	 */
	static var DefaultCachePolicy(default, default):cs.system.net.cache.RequestCachePolicy;
	/**
	 * Gets or sets the global HTTP proxy.
	 * @return An  used by every call to instances of .
	 */
	static var DefaultWebProxy(default, default):cs.system.net.IWebProxy;
	/**
	 * Gets or sets values indicating the level of authentication and impersonation
	 * used for this request.
	 * @return A bitwise combination of the  values. The default value is . In mutual
	 * authentication, both the client and server present credentials to establish
	 * their identity. The  and  values are relevant for Kerberos authentication.
	 * Kerberos authentication can be supported directly, or can be used if the
	 * Negotiate security protocol is used to select the actual security protocol. For
	 * more information about authentication protocols, see Internet Authentication. To
	 * determine whether mutual authentication occurred, check the  property. If you
	 * specify the  authentication flag value and mutual authentication does not occur,
	 * your application will receive an  with a  inner exception indicating that mutual
	 * authentication failed.
	 */
	var AuthenticationLevel(default, default):cs.system.net.security.AuthenticationLevel;
	/**
	 * Gets or sets the cache policy for this request.
	 * @return A  object that defines a cache policy.
	 */
	var CachePolicy(default, default):cs.system.net.cache.RequestCachePolicy;
	/**
	 * When overridden in a descendant class, gets or sets the name of the connection
	 * group for the request.
	 * @return The name of the connection group for the request.
	 */
	var ConnectionGroupName(default, default):String;
	/**
	 * When overridden in a descendant class, gets or sets the content length of the
	 * request data being sent.
	 * @return The number of bytes of request data being sent.
	 */
	var ContentLength(default, default):haxe.Int64;
	/**
	 * When overridden in a descendant class, gets or sets the content type of the
	 * request data being sent.
	 * @return The content type of the request data.
	 */
	var ContentType(default, default):String;
	/**
	 * When overridden in a descendant class, gets or sets the network credentials used
	 * for authenticating the request with the Internet resource.
	 * @return An  containing the authentication credentials associated with the
	 * request. The default is .
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * When overridden in a descendant class, gets or sets the collection of header
	 * name/value pairs associated with the request.
	 * @return A  containing the header name/value pairs associated with this request.
	 */
	var Headers(default, default):cs.system.net.WebHeaderCollection;
	/**
	 * Gets or sets the impersonation level for the current request.
	 * @return A  value.
	 */
	var ImpersonationLevel(default, default):cs.system.security.principal.TokenImpersonationLevel;
	/**
	 * When overridden in a descendant class, gets or sets the protocol method to use
	 * in this request.
	 * @return The protocol method to use in this request.
	 */
	var Method(default, default):String;
	/**
	 * When overridden in a descendant class, indicates whether to pre-authenticate the
	 * request.
	 * @return to pre-authenticate; otherwise, .
	 */
	var PreAuthenticate(default, default):Bool;
	/**
	 * When overridden in a descendant class, gets or sets the network proxy to use to
	 * access this Internet resource.
	 * @return The  to use to access the Internet resource.
	 */
	var Proxy(default, default):cs.system.net.IWebProxy;
	/**
	 * When overridden in a descendant class, gets the URI of the Internet resource
	 * associated with the request.
	 * @return A  representing the resource associated with the request
	 */
	var RequestUri(default, never):cs.system.Uri;
	/**
	 * Gets or sets the length of time, in milliseconds, before the request times out.
	 * @return The length of time, in milliseconds, until the request times out, or the
	 * value  to indicate that the request does not time out. The default value is
	 * defined by the descendant class.
	 */
	var Timeout(default, default):Int;
	/**
	 * When overridden in a descendant class, gets or sets a  value that controls
	 * whether  are sent with requests.
	 * @return if the default credentials are used; otherwise . The default value is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	@:overload(function(requestUriString:String):cs.system.net.WebRequest {})
	/**
	 * Initializes a new  instance for the specified URI scheme.
	 * @param requestUriString The URI that identifies the Internet resource.
	 * @return A  descendant for the specific URI scheme.
	 */
	static function Create(requestUri:cs.system.Uri):cs.system.net.WebRequest;
	/**
	 * Initializes a new  instance for the specified URI scheme.
	 * @param requestUri A  containing the URI of the requested resource.
	 * @return A  descendant for the specified URI scheme.
	 */
	static function CreateDefault(requestUri:cs.system.Uri):cs.system.net.WebRequest;
	@:overload(function(requestUriString:String):cs.system.net.HttpWebRequest {})
	/**
	 * Initializes a new  instance for the specified URI string.
	 * @param requestUriString A URI string that identifies the Internet resource.
	 * @return An  instance for the specific URI string.
	 */
	static function CreateHttp(requestUri:cs.system.Uri):cs.system.net.HttpWebRequest;
	/**
	 * Returns a proxy configured with the Internet Explorer settings of the currently
	 * impersonated user.
	 * @return An  used by every call to instances of .
	 */
	static function GetSystemWebProxy():cs.system.net.IWebProxy;
	/**
	 * Registers a  descendant for the specified URI.
	 * @param prefix The complete URI or URI prefix that the  descendant services.
	 * @param creator The create method that the  calls to create the  descendant.
	 * @return if registration is successful; otherwise, .
	 */
	static function RegisterPrefix(prefix:String, creator:cs.system.net.IWebRequestCreate):Bool;
	/** Aborts the request. */
	function Abort():Void;
	/**
	 * When overridden in a descendant class, provides an asynchronous version of the 
	 * method.
	 * @param callback The  delegate.
	 * @param state An object containing state information for this asynchronous
	 * request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetRequestStream(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * When overridden in a descendant class, begins an asynchronous request for an
	 * Internet resource.
	 * @param callback The  delegate.
	 * @param state An object containing state information for this asynchronous
	 * request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetResponse(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * When overridden in a descendant class, returns a  for writing data to the
	 * Internet resource.
	 * @param asyncResult An  that references a pending request for a stream.
	 * @return A  to write data to.
	 */
	function EndGetRequestStream(asyncResult:cs.system.IAsyncResult):cs.system.io.Stream;
	/**
	 * When overridden in a descendant class, returns a .
	 * @param asyncResult An  that references a pending request for a response.
	 * @return A  that contains a response to the Internet request.
	 */
	function EndGetResponse(asyncResult:cs.system.IAsyncResult):cs.system.net.WebResponse;
	/**
	 * When overridden in a descendant class, returns a  for writing data to the
	 * Internet resource.
	 * @return A  for writing data to the Internet resource.
	 */
	function GetRequestStream():cs.system.io.Stream;
	/**
	 * When overridden in a descendant class, returns a  for writing data to the
	 * Internet resource as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetRequestStreamAsync():cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	/**
	 * When overridden in a descendant class, returns a response to an Internet
	 * request.
	 * @return A  containing the response to the Internet request.
	 */
	function GetResponse():cs.system.net.WebResponse;
	/**
	 * When overridden in a descendant class, returns a response to an Internet request
	 * as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetResponseAsync():cs.system.threading.tasks.Task_1<cs.system.net.WebResponse>;
}
