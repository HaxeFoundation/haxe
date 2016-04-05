package sys.ssl;

/**
	A TLS socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
extern class Socket extends sys.net.Socket {

	static var DEFAULT_VERIFY_CERT : Null<Bool>;

	static var DEFAULT_CA : Null<sys.ssl.Certificate>;

	/**
		Define if peer certificate is verified during SSL handshake.
	**/
	var verifyCert : Null<Bool>;

	function new() : Void;

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
		Configure additionals certificates and private keys for Server Name Indication extension.
		The callback may be called during handshake to determine the certificate to use.
	**/
	function addSNICertificate( cbServernameMatch : String->Bool, cert : Certificate, key : Key ) : Void;
	
	/**
		Return the certificate received from the other side of a connection.
	**/
	function peerCertificate() : sys.ssl.Certificate;

}
