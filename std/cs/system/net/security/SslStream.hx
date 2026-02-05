package cs.system.net.security;

/** Provides a stream used for client-server communication that uses the Secure Socket Layer (SSL) security protocol to authenticate the server and optionally the client. */
@:native("System.Net.Security.SslStream")
extern class SslStream extends cs.system.net.security.AuthenticatedStream {
	/**
	 * Gets a  value that indicates whether the certificate revocation list is checked
	 * during the certificate validation process.
	 * @return if the certificate revocation list is checked; otherwise, .
	 */
	var CheckCertRevocationStatus(default, never):Bool;
	/**
	 * Gets a value that identifies the bulk encryption algorithm used by this .
	 * @return A value that identifies the bulk encryption algorithm used by this .
	 */
	var CipherAlgorithm(default, never):cs.system.security.authentication.CipherAlgorithmType;
	/**
	 * Gets a value that identifies the strength of the cipher algorithm used by this .
	 * @return An  value that specifies the strength of the algorithm, in bits.
	 */
	var CipherStrength(default, never):Int;
	/**
	 * Gets the algorithm used for generating message authentication codes (MACs).
	 * @return The algorithm used for generating message authentication codes (MACs).
	 */
	var HashAlgorithm(default, never):cs.system.security.authentication.HashAlgorithmType;
	/**
	 * Gets a value that identifies the strength of the hash algorithm used by this
	 * instance.
	 * @return An  value that specifies the strength of the  algorithm, in bits. Valid
	 * values are 128 or 160.
	 */
	var HashStrength(default, never):Int;
	/**
	 * Gets the key exchange algorithm used by this .
	 * @return An  value.
	 */
	var KeyExchangeAlgorithm(default, never):cs.system.security.authentication.ExchangeAlgorithmType;
	/**
	 * Gets a value that identifies the strength of the key exchange algorithm used by
	 * this instance.
	 * @return An  value that specifies the strength of the  algorithm, in bits.
	 */
	var KeyExchangeStrength(default, never):Int;
	/**
	 * Gets the certificate used to authenticate the local endpoint.
	 * @return An X509Certificate object that represents the certificate supplied for
	 * authentication or  if no certificate was supplied.
	 */
	var LocalCertificate(default, never):cs.system.security.cryptography.x509certificates.X509Certificate;
	var NegotiatedApplicationProtocol(default, never):cs.system.net.security.SslApplicationProtocol;
	/**
	 * Gets the certificate used to authenticate the remote endpoint.
	 * @return An X509Certificate object that represents the certificate supplied for
	 * authentication or  if no certificate was supplied.
	 */
	var RemoteCertificate(default, never):cs.system.security.cryptography.x509certificates.X509Certificate;
	/**
	 * Gets a value that indicates the security protocol used to authenticate this
	 * connection.
	 * @return The  value that represents the protocol used for authentication.
	 */
	var SslProtocol(default, never):cs.system.security.authentication.SslProtocols;
	/**
	 * Gets the  used for authentication using extended protection.
	 * @return The  object that contains the channel binding token (CBT) used for
	 * extended protection.
	 */
	var TransportContext(default, never):cs.system.net.TransportContext;
	@:overload(function(innerStream:cs.system.io.Stream):Void {})
	@:overload(function(innerStream:cs.system.io.Stream, leaveInnerStreamOpen:Bool):Void {})
	@:overload(function(innerStream:cs.system.io.Stream, leaveInnerStreamOpen:Bool, userCertificateValidationCallback:cs.system.net.security.RemoteCertificateValidationCallback):Void {})
	@:overload(function(innerStream:cs.system.io.Stream, leaveInnerStreamOpen:Bool, userCertificateValidationCallback:cs.system.net.security.RemoteCertificateValidationCallback, userCertificateSelectionCallback:cs.system.net.security.LocalCertificateSelectionCallback):Void {})
	function new(innerStream:cs.system.io.Stream, leaveInnerStreamOpen:Bool, userCertificateValidationCallback:cs.system.net.security.RemoteCertificateValidationCallback, userCertificateSelectionCallback:cs.system.net.security.LocalCertificateSelectionCallback, encryptionPolicy:cs.system.net.security.EncryptionPolicy):Void;
	@:overload(function(targetHost:String):Void {})
	@:overload(function(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, checkCertificateRevocation:Bool):Void {})
	/**
	 * Called by clients to authenticate the server and optionally the client in a
	 * client-server connection.
	 * @param targetHost The name of the server that shares this .
	 */
	function AuthenticateAsClient(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool):Void;
	@:overload(function(targetHost:String):cs.system.threading.tasks.Task {})
	@:overload(function(sslClientAuthenticationOptions:cs.system.net.security.SslClientAuthenticationOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, checkCertificateRevocation:Bool):cs.system.threading.tasks.Task {})
	/**
	 * @param sslClientAuthenticationOptions 
	 * @param cancellationToken 
	 */
	function AuthenticateAsClientAsync(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool):cs.system.threading.tasks.Task;
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate):Void {})
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, checkCertificateRevocation:Bool):Void {})
	/**
	 * Called by servers to authenticate the server and optionally the client in a
	 * client-server connection using the specified certificate.
	 * @param serverCertificate The certificate used to authenticate the server.
	 */
	function AuthenticateAsServer(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool):Void;
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate):cs.system.threading.tasks.Task {})
	@:overload(function(sslServerAuthenticationOptions:cs.system.net.security.SslServerAuthenticationOptions, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, checkCertificateRevocation:Bool):cs.system.threading.tasks.Task {})
	/**
	 * @param sslServerAuthenticationOptions 
	 * @param cancellationToken 
	 */
	function AuthenticateAsServerAsync(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool):cs.system.threading.tasks.Task;
	@:overload(function(targetHost:String, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, checkCertificateRevocation:Bool, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Called by clients to begin an asynchronous operation to authenticate the server
	 * and optionally the client.
	 * @param targetHost The name of the server that shares this .
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * authentication is complete.
	 * @param asyncState A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object that indicates the status of the asynchronous operation.
	 */
	function BeginAuthenticateAsClient(targetHost:String, clientCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, checkCertificateRevocation:Bool, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Called by servers to begin an asynchronous operation to authenticate the client
	 * and optionally the server in a client-server connection.
	 * @param serverCertificate The X509Certificate used to authenticate the server.
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * authentication is complete.
	 * @param asyncState A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginAuthenticateAsServer(serverCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, clientCertificateRequired:Bool, enabledSslProtocols:cs.system.security.authentication.SslProtocols, checkCertificateRevocation:Bool, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous read operation that reads data from the stream and stores
	 * it in the specified array.
	 * @param buffer A  array that receives the bytes read from the stream.
	 * @param offset The zero-based location in  at which to begin storing the data
	 * read from this stream.
	 * @param count The maximum number of bytes to read from the stream.
	 * @param asyncCallback An  delegate that references the method to invoke when the
	 * read operation is complete.
	 * @param asyncState A user-defined object that contains information about the read
	 * operation. This object is passed to the  delegate when the operation completes.
	 * @return An  object that indicates the status of the asynchronous operation.
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
	 * @param asyncState A user-defined object that contains information about the
	 * write operation. This object is passed to the  delegate when the operation
	 * completes.
	 * @return An  object indicating the status of the asynchronous operation.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Asynchronously releases the unmanaged and managed resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Ends a pending asynchronous server authentication operation started with a
	 * previous call to .
	 * @param asyncResult An  instance returned by a call to .
	 */
	function EndAuthenticateAsClient(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends a pending asynchronous client authentication operation started with a
	 * previous call to .
	 * @param asyncResult An  instance returned by a call to .
	 */
	function EndAuthenticateAsServer(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends an asynchronous read operation started with a previous call to .
	 * @param asyncResult An  instance returned by a call to
	 * @return A  value that specifies the number of bytes read from the underlying
	 * stream.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation started with a previous call to .
	 * @param asyncResult An  instance returned by a call to
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/** @param cancellationToken  */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Reads data from this stream and stores it in the specified array.
	 * @param buffer A  array that receives the bytes read from this stream.
	 * @param offset A  that contains the zero-based location in  at which to begin
	 * storing the data read from this stream.
	 * @param count A  that contains the maximum number of bytes to read from this
	 * stream.
	 * @return A  value that specifies the number of bytes read. When there is no more
	 * data to be read, returns 0.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	function ReadByte():Int;
	/**
	 * Throws a .
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
	 * Shuts down this SslStream.
	 * @return The task object representing the asynchronous operation.
	 */
	function ShutdownAsync():cs.system.threading.tasks.Task;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * Writes the specified data to this stream.
	 * @param buffer A  array that supplies the bytes written to the stream.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
