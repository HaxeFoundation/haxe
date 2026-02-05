package cs.system.net;

/** Provides a simple, programmatically controlled HTTP protocol listener. This class cannot be inherited. */
@:native("System.Net.HttpListener")
extern class HttpListener {
	/**
	 * Gets a value that indicates whether  can be used with the current operating
	 * system.
	 * @return if  is supported; otherwise, .
	 */
	static var IsSupported(default, never):Bool;
	/**
	 * Gets or sets the scheme used to authenticate clients.
	 * @return A bitwise combination of  enumeration values that indicates how clients
	 * are to be authenticated. The default value is .
	 */
	var AuthenticationSchemes(default, default):cs.system.net.AuthenticationSchemes;
	/**
	 * Gets or sets the delegate called to determine the protocol used to authenticate
	 * clients.
	 * @return An  delegate that invokes the method used to select an authentication
	 * protocol. The default value is .
	 */
	var AuthenticationSchemeSelectorDelegate(default, default):cs.system.net.AuthenticationSchemeSelector;
	/**
	 * Gets a default list of Service Provider Names (SPNs) as determined by registered
	 * prefixes.
	 * @return A  that contains a list of SPNs.
	 */
	var DefaultServiceNames(default, never):cs.system.security.authentication.extendedprotection.ServiceNameCollection;
	/**
	 * Gets or sets the  to use for extended protection for a session.
	 * @return A  that specifies the policy to use for extended protection.
	 */
	var ExtendedProtectionPolicy(default, default):cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy;
	/**
	 * Gets or sets the delegate called to determine the  to use for each request.
	 * @return A  that specifies the policy to use for extended protection.
	 */
	var ExtendedProtectionSelectorDelegate(default, default):cs.system.net.HttpListener_ExtendedProtectionSelector;
	/**
	 * Gets or sets a  value that specifies whether your application receives
	 * exceptions that occur when an  sends the response to the client.
	 * @return if this  should not return exceptions that occur when sending the
	 * response to the client; otherwise, . The default value is .
	 */
	var IgnoreWriteExceptions(default, default):Bool;
	/**
	 * Gets a value that indicates whether  has been started.
	 * @return if the  was started; otherwise, .
	 */
	var IsListening(default, never):Bool;
	/**
	 * Gets the Uniform Resource Identifier (URI) prefixes handled by this  object.
	 * @return An  that contains the URI prefixes that this  object is configured to
	 * handle.
	 */
	var Prefixes(default, never):cs.system.net.HttpListenerPrefixCollection;
	/**
	 * Gets or sets the realm, or resource partition, associated with this  object.
	 * @return A  value that contains the name of the realm associated with the 
	 * object.
	 */
	var Realm(default, default):String;
	/**
	 * The timeout manager for this  instance.
	 * @return The timeout manager for this  instance.
	 */
	var TimeoutManager(default, never):cs.system.net.HttpListenerTimeoutManager;
	/**
	 * Gets or sets a  value that controls whether, when NTLM is used, additional
	 * requests using the same Transmission Control Protocol (TCP) connection are
	 * required to authenticate.
	 * @return if the  of the first request will be used for subsequent requests on the
	 * same connection; otherwise, . The default value is .
	 */
	var UnsafeConnectionNtlmAuthentication(default, default):Bool;
	function new():Void;
	/** Shuts down the  object immediately, discarding all currently queued requests. */
	function Abort():Void;
	/**
	 * Begins asynchronously retrieving an incoming request.
	 * @param callback An  delegate that references the method to invoke when a client
	 * request is available.
	 * @param state A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object that indicates the status of the asynchronous operation.
	 */
	function BeginGetContext(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/** Shuts down the . */
	function Close():Void;
	/**
	 * Completes an asynchronous operation to retrieve an incoming client request.
	 * @param asyncResult An  object that was obtained when the asynchronous operation
	 * was started.
	 * @return An  object that represents the client request.
	 */
	function EndGetContext(asyncResult:cs.system.IAsyncResult):cs.system.net.HttpListenerContext;
	/**
	 * Waits for an incoming request and returns when one is received.
	 * @return An  object that represents a client request.
	 */
	function GetContext():cs.system.net.HttpListenerContext;
	/**
	 * Waits for an incoming request as an asynchronous operation.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns an  object that represents a client request.
	 */
	function GetContextAsync():cs.system.threading.tasks.Task_1<cs.system.net.HttpListenerContext>;
	/** Allows this instance to receive incoming requests. */
	function Start():Void;
	/** Causes this instance to stop receiving incoming requests. */
	function Stop():Void;
}
