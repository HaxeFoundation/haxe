package cs.system.net;

/** Implements a File Transfer Protocol (FTP) client. */
@:native("System.Net.FtpWebRequest")
extern class FtpWebRequest extends cs.system.net.WebRequest {
	/**
	 * Gets or sets the certificates used for establishing an encrypted connection to
	 * the FTP server.
	 * @return An  object that contains the client certificates.
	 */
	var ClientCertificates(default, default):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	/**
	 * Gets or sets a byte offset into the file being downloaded by this request.
	 * @return An  instance that specifies the file offset, in bytes. The default value
	 * is zero.
	 */
	var ContentOffset(default, default):haxe.Int64;
	/**
	 * Gets or sets a  that specifies that an SSL connection should be used.
	 * @return if control and data transmissions are encrypted; otherwise, . The
	 * default value is .
	 */
	var EnableSsl(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the control connection to the FTP
	 * server is closed after the request completes.
	 * @return if the connection to the server should not be destroyed; otherwise, .
	 * The default value is .
	 */
	var KeepAlive(default, default):Bool;
	/**
	 * Gets or sets a time-out when reading from or writing to a stream.
	 * @return The number of milliseconds before the reading or writing times out. The
	 * default value is 300,000 milliseconds (5 minutes).
	 */
	var ReadWriteTimeout(default, default):Int;
	/**
	 * Gets or sets the new name of a file being renamed.
	 * @return The new name of the file being renamed.
	 */
	var RenameTo(default, default):String;
	/**
	 * Gets the  object used to connect to the FTP server.
	 * @return A  object that can be used to customize connection behavior.
	 */
	var ServicePoint(default, never):cs.system.net.ServicePoint;
	/**
	 * Gets or sets a  value that specifies the data type for file transfers.
	 * @return to indicate to the server that the data to be transferred is binary;  to
	 * indicate that the data is text. The default value is .
	 */
	var UseBinary(default, default):Bool;
	/**
	 * Gets or sets the behavior of a client application's data transfer process.
	 * @return if the client application's data transfer process listens for a
	 * connection on the data port; otherwise,  if the client should initiate a
	 * connection on the data port. The default value is .
	 */
	var UsePassive(default, default):Bool;
	/** Terminates an asynchronous FTP operation. */
	function Abort():Void;
	/**
	 * Begins asynchronously opening a request's content stream for writing.
	 * @param callback An  delegate that references the method to invoke when the
	 * operation is complete.
	 * @param state A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  instance that indicates the status of the operation.
	 */
	function BeginGetRequestStream(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins sending a request and receiving a response from an FTP server
	 * asynchronously.
	 * @param callback An  delegate that references the method to invoke when the
	 * operation is complete.
	 * @param state A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  instance that indicates the status of the operation.
	 */
	function BeginGetResponse(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends a pending asynchronous operation started with .
	 * @param asyncResult The  object that was returned when the operation started.
	 * @return A writable  instance associated with this instance.
	 */
	function EndGetRequestStream(asyncResult:cs.system.IAsyncResult):cs.system.io.Stream;
	/**
	 * Ends a pending asynchronous operation started with .
	 * @param asyncResult The  that was returned when the operation started.
	 * @return A  reference that contains an  instance. This object contains the FTP
	 * server's response to the request.
	 */
	function EndGetResponse(asyncResult:cs.system.IAsyncResult):cs.system.net.WebResponse;
	/**
	 * Retrieves the stream used to upload data to an FTP server.
	 * @return A writable  instance used to store data to be sent to the server by the
	 * current request.
	 */
	function GetRequestStream():cs.system.io.Stream;
	/**
	 * Returns the FTP server response.
	 * @return A  reference that contains an  instance. This object contains the FTP
	 * server's response to the request.
	 */
	function GetResponse():cs.system.net.WebResponse;
}
