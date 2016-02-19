package sys.ssl;

/**
	A TLS socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
extern class Socket implements sys.net.ISocket {

	static var DEFAULT_VERIFY_CERT : Null<Bool>;

	static var DEFAULT_CA : Null<sys.ssl.Certificate>;
	
	/**
		The stream on which you can read available data. By default the stream is blocking until the requested data is available,
		use [setBlocking(false)] or [setTimeout] to prevent infinite waiting.
	**/	
	var input(default,null) : haxe.io.Input;

	/**
		The stream on which you can send data. Please note that in case the output buffer you will block while writing the data, use [setBlocking(false)] or [setTimeout] to prevent that.
	**/
	var output(default, null) : haxe.io.Output;
	
	/**
		A custom value that can be associated with the socket. Can be used to retreive your custom infos after a [select].
	***/
	var custom : Dynamic;
	
	/**
		Define if peer certificate is verified during SSL handshake.
	**/
	var verifyCert : Null<Bool>;

	/**
		Creates a new unconnected socket.
	**/
	function new();

	/**
		Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
	**/
	function close() : Void;
	
	/**
		Connect to the given server host/port. Throw an exception in case we couldn't sucessfully connect.
		In blocking mode the SSL handshake is done automatically.
	**/
	function connect(host : sys.net.Host, port : Int) : Void;

	/**
		Perform the SSL handshake.
	**/
	function handshake() : Void;
	
	/**
		Configure the certificate chain for peer certificate verification.
	**/	
	function setCA( cert : sys.ssl.Certificate ) : Void;

	/**
		Configure the hostname for Server Name Indication TLS extension.
	**/
	function setHostname( name : String ) : Void;

	/**
		Configure own certificate and private key.
	**/
	function setCertificate( cert : Certificate, key : Key ) : Void;

	/**
		Read the whole data available on the socket.
	**/
	function read() : String;
	
	/**
		Write the whole data to the socket output.
	**/
	function write( content : String ) : Void;
	
	/**
		Configure additionals certificates and private keys for Server Name Indication extension.
		The callback may be called during handshake to determine the certificate to use.
	**/
	function addSNICertificate( cbServernameMatch : String->Bool, cert : Certificate, key : Key ) : Void;
	
	/**
		Bind the socket to the given host/port so it can afterwards listen for connections there.
	**/
	function bind( host : sys.net.Host, port : Int ) : Void;

	/**
		Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use [accept()] to accept incoming connections.
	**/
	function listen( connections : Int ) : Void;
	
	/**
		Accept a new connected client. This will return a connected socket on which you must call [handshake()] before read/write some data.
	**/
	function accept() : Socket;

	/**
		Return the informations about the other side of a connected socket.
	**/
	function peer() : { host : sys.net.Host, port : Int };

	/**
		Return the certificate received from the other side of a connection.
	**/
	function peerCertificate() : sys.ssl.Certificate;

	/**
		Shutdown the socket, either for reading or writing.
	**/
	function shutdown( read : Bool, write : Bool ) : Void;
	
	/**
		Return the informations about our side of a connected socket.
	**/
	function host() : { host : sys.net.Host, port : Int };

	/**
		Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
	**/
	function setTimeout( timeout : Float ) : Void;

	/**
		Block until some data is available for read on the socket.
	**/
	function waitForRead() : Void;
	
	/**
		Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediatly by throwing a haxe.io.Error.Blocking value.
		If you call [setBlocking(false)] before [connect()], you need to call [handshake()] before read/write some data.
	**/
	function setBlocking( b : Bool ) : Void;
	
	/**
		Allows the socket to immediatly send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
	**/
	function setFastSend( b : Bool ) : Void;
	
}
