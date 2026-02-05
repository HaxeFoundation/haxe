package cs.system.net.security;

/** Provides a stream that uses the Negotiate security protocol to authenticate the client, and optionally the server, in client-server communication. */
@:native("System.Net.Security.NegotiateStream")
extern class NegotiateStream extends cs.system.net.security.AuthenticatedStream {
	/**
	 * Gets a value that indicates how the server can use the client's credentials.
	 * @return One of the  values.
	 */
	var ImpersonationLevel(default, never):cs.system.security.principal.TokenImpersonationLevel;
	/**
	 * Gets information about the identity of the remote party sharing this
	 * authenticated stream.
	 * @return An  object that describes the identity of the remote endpoint.
	 */
	var RemoteIdentity(default, never):cs.system.security.principal.IIdentity;
	@:overload(function(innerStream:cs.system.io.Stream):Void {})
	function new(innerStream:cs.system.io.Stream, leaveInnerStreamOpen:Bool):Void;
	@:overload(function():Void {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String):Void {})
	@:overload(function(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String):Void {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):Void {})
	/** Called by clients to authenticate the client, and optionally the server, in a client-server connection. */
	function AuthenticateAsClient(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String):cs.system.threading.tasks.Task {})
	@:overload(function(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String):cs.system.threading.tasks.Task {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):cs.system.threading.tasks.Task {})
	/**
	 * Called by clients to authenticate the client, and optionally the server, in a
	 * client-server connection as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function AuthenticateAsClientAsync(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):cs.system.threading.tasks.Task;
	@:overload(function():Void {})
	@:overload(function(policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy):Void {})
	@:overload(function(credential:cs.system.net.NetworkCredential, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):Void {})
	/** Called by servers to authenticate the client, and optionally the server, in a client-server connection. */
	function AuthenticateAsServer(credential:cs.system.net.NetworkCredential, policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy):cs.system.threading.tasks.Task {})
	@:overload(function(credential:cs.system.net.NetworkCredential, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):cs.system.threading.tasks.Task {})
	/**
	 * Called by servers to authenticate the client, and optionally the server, in a
	 * client-server connection as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function AuthenticateAsServerAsync(credential:cs.system.net.NetworkCredential, policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel):cs.system.threading.tasks.Task;
	@:overload(function(asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(credential:cs.system.net.NetworkCredential, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Called by clients to begin an asynchronous operation to authenticate the client,
	 * and optionally the server, in a client-server connection. This method does not
	 * block.
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * authentication is complete.
	 * @param asyncState A user-defined object containing information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginAuthenticateAsClient(credential:cs.system.net.NetworkCredential, binding:cs.system.security.authentication.extendedprotection.ChannelBinding, targetName:String, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, allowedImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	@:overload(function(asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(credential:cs.system.net.NetworkCredential, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Called by servers to begin an asynchronous operation to authenticate the client,
	 * and optionally the server, in a client-server connection. This method does not
	 * block.
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * authentication is complete.
	 * @param asyncState A user-defined object containing information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginAuthenticateAsServer(credential:cs.system.net.NetworkCredential, policy:cs.system.security.authentication.extendedprotection.ExtendedProtectionPolicy, requiredProtectionLevel:cs.system.net.security.ProtectionLevel, requiredImpersonationLevel:cs.system.security.principal.TokenImpersonationLevel, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous read operation that reads data from the stream and stores
	 * it in the specified array.
	 * @param buffer A  array that receives the bytes read from the stream.
	 * @param offset The zero-based location in  at which to begin storing the data
	 * read from this stream.
	 * @param count The maximum number of bytes to read from the stream.
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * read operation is complete.
	 * @param asyncState A user-defined object containing information about the read
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation that writes s from the specified buffer
	 * to the stream.
	 * @param buffer A  array that supplies the bytes to be written to the stream.
	 * @param offset The zero-based location in  at which to begin reading bytes to be
	 * written to the stream.
	 * @param count An  value that specifies the number of bytes to read from .
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * write operation is complete.
	 * @param asyncState A user-defined object containing information about the write
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Asynchronously releases the unmanaged and managed resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Ends a pending asynchronous client authentication operation that was started
	 * with a call to .
	 * @param asyncResult An  instance returned by a call to .
	 */
	function EndAuthenticateAsClient(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends a pending asynchronous client authentication operation that was started
	 * with a call to .
	 * @param asyncResult An  instance returned by a call to .
	 */
	function EndAuthenticateAsServer(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends an asynchronous read operation that was started with a call to .
	 * @param asyncResult An  instance returned by a call to
	 * @return A  value that specifies the number of bytes read from the underlying
	 * stream.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation that was started with a call to .
	 * @param asyncResult An  instance returned by a call to
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/** @param cancellationToken  */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Reads data from this stream and stores it in the specified array.
	 * @param buffer A  array that receives the bytes read from the stream.
	 * @param offset A  containing the zero-based location in  at which to begin
	 * storing the data read from this stream.
	 * @param count A  containing the maximum number of bytes to read from the stream.
	 * @return A  value that specifies the number of bytes read from the underlying
	 * stream. When there is no more data to be read, returns 0.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	/**
	 * Throws .
	 * @param offset This value is ignored.
	 * @param origin This value is ignored.
	 * @return Always throws a .
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the underlying stream.
	 * @param value An  value that specifies the length of the stream.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Write the specified number of s to the underlying stream using the specified
	 * buffer and offset.
	 * @param buffer A  array that supplies the bytes written to the stream.
	 * @param offset An  containing the zero-based location in  at which to begin
	 * reading bytes to be written to the stream.
	 * @param count A  containing the number of bytes to read from .
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
}
