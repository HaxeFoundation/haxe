package cs.system.net;

/** Encapsulates a File Transfer Protocol (FTP) server's response to a request. */
@:native("System.Net.FtpWebResponse")
extern class FtpWebResponse extends cs.system.net.WebResponse {
	/**
	 * Gets the message sent by the FTP server when a connection is established prior
	 * to logon.
	 * @return A  that contains the banner message sent by the server; otherwise,  if
	 * no message is sent.
	 */
	var BannerMessage(default, never):String;
	/**
	 * Gets the message sent by the server when the FTP session is ending.
	 * @return A  that contains the exit message sent by the server; otherwise,  if no
	 * message is sent.
	 */
	var ExitMessage(default, never):String;
	/**
	 * Gets the date and time that a file on an FTP server was last modified.
	 * @return A  that contains the last modified date and time for a file.
	 */
	var LastModified(default, never):cs.system.DateTime;
	/**
	 * Gets the most recent status code sent from the FTP server.
	 * @return An  value that indicates the most recent status code returned with this
	 * response.
	 */
	var StatusCode(default, never):cs.system.net.FtpStatusCode;
	/**
	 * Gets text that describes a status code sent from the FTP server.
	 * @return A  instance that contains the status code and message returned with this
	 * response.
	 */
	var StatusDescription(default, never):String;
	/**
	 * Gets the message sent by the FTP server when authentication is complete.
	 * @return A  that contains the welcome message sent by the server; otherwise,  if
	 * no message is sent.
	 */
	var WelcomeMessage(default, never):String;
	/** Frees the resources held by the response. */
	function Close():Void;
	/**
	 * Retrieves the stream that contains response data sent from an FTP server.
	 * @return A readable  instance that contains data returned with the response;
	 * otherwise,  if no response data was returned by the server.
	 */
	function GetResponseStream():cs.system.io.Stream;
}
