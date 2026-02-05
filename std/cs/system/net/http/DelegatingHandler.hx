package cs.system.net.http;

/** A type for HTTP handlers that delegate the processing of HTTP response messages to another handler, called the inner handler. */
@:native("System.Net.Http.DelegatingHandler")
extern class DelegatingHandler extends cs.system.net.http.HttpMessageHandler {
	/**
	 * Gets or sets the inner handler which processes the HTTP response messages.
	 * @return The inner handler for HTTP response messages.
	 */
	var InnerHandler(default, default):cs.system.net.http.HttpMessageHandler;
}
