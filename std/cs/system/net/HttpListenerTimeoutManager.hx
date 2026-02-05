package cs.system.net;

/** The timeout manager to use for an  object. */
@:native("System.Net.HttpListenerTimeoutManager")
extern class HttpListenerTimeoutManager {
	/**
	 * Gets or sets the time allowed for the  to drain the entity body on a Keep-Alive
	 * connection.
	 * @return The time allowed for the  to drain the entity body on a Keep-Alive
	 * connection.
	 */
	var DrainEntityBody(default, default):cs.system.TimeSpan;
	/**
	 * Gets or sets the time allowed for the request entity body to arrive.
	 * @return The time allowed for the request entity body to arrive.
	 */
	var EntityBody(default, default):cs.system.TimeSpan;
	/**
	 * Gets or sets the time allowed for the  to parse the request header.
	 * @return The time allowed for the  to parse the request header.
	 */
	var HeaderWait(default, default):cs.system.TimeSpan;
	/**
	 * Gets or sets the time allowed for an idle connection.
	 * @return The time allowed for an idle connection.
	 */
	var IdleConnection(default, default):cs.system.TimeSpan;
	/**
	 * Gets or sets the minimum send rate, in bytes-per-second, for the response.
	 * @return The minimum send rate, in bytes-per-second, for the response.
	 */
	var MinSendBytesPerSecond(default, default):haxe.Int64;
	/**
	 * Gets or sets the time allowed for the request to remain in the request queue
	 * before the  picks it up.
	 * @return The time allowed for the request to remain in the request queue before
	 * the  picks it up.
	 */
	var RequestQueue(default, default):cs.system.TimeSpan;
}
