package cs.system.net.security;

/** Provides methods for passing credentials across a stream and requesting or performing authentication for client-server applications. */
@:native("System.Net.Security.AuthenticatedStream")
extern class AuthenticatedStream extends cs.system.io.Stream {
	/**
	 * Gets the stream used by this  for sending and receiving data.
	 * @return A  object.
	 */
	var InnerStream(default, never):cs.system.io.Stream;
	/**
	 * Gets a  value that indicates whether authentication was successful.
	 * @return if successful authentication occurred; otherwise, .
	 */
	var IsAuthenticated(default, never):Bool;
	/**
	 * Gets a  value that indicates whether data sent using this  is encrypted.
	 * @return if data is encrypted before being transmitted over the network and
	 * decrypted when it reaches the remote endpoint; otherwise, .
	 */
	var IsEncrypted(default, never):Bool;
	/**
	 * Gets a  value that indicates whether both server and client have been
	 * authenticated.
	 * @return if the client and server have been authenticated; otherwise, .
	 */
	var IsMutuallyAuthenticated(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the local side of the connection was
	 * authenticated as the server.
	 * @return if the local endpoint was authenticated as the server side of a
	 * client-server authenticated connection;  if the local endpoint was authenticated
	 * as the client.
	 */
	var IsServer(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the data sent using this stream is signed.
	 * @return if the data is signed before being transmitted; otherwise, .
	 */
	var IsSigned(default, never):Bool;
	/**
	 * Gets whether the stream used by this  for sending and receiving data has been
	 * left open.
	 * @return if the inner stream has been left open; otherwise, .
	 */
	var LeaveInnerStreamOpen(default, never):Bool;
	/**
	 * Asynchronously releases the unmanaged and managed resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
}
