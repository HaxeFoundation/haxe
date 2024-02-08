/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package sys.ssl;

/**
	A TLS socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
extern class Socket extends sys.net.Socket {
	static var DEFAULT_VERIFY_CERT:Null<Bool>;

	static var DEFAULT_CA:Null<sys.ssl.Certificate>;

	/**
		Define if peer certificate is verified during SSL handshake.
	**/
	var verifyCert:Null<Bool>;

	function new():Void;

	/**
		Perform the SSL handshake.
	**/
	function handshake():Void;

	/**
		Configure the certificate chain for peer certificate verification.
	**/
	function setCA(cert:sys.ssl.Certificate):Void;

	/**
		Configure the hostname for Server Name Indication TLS extension.
	**/
	function setHostname(name:String):Void;

	/**
		Configure own certificate and private key.
	**/
	function setCertificate(cert:Certificate, key:Key):Void;

	/**
		Configure additionals certificates and private keys for Server Name Indication extension.
		The callback may be called during handshake to determine the certificate to use.
	**/
	function addSNICertificate(cbServernameMatch:String->Bool, cert:Certificate, key:Key):Void;

	/**
		Return the certificate received from the other side of a connection.
	**/
	function peerCertificate():sys.ssl.Certificate;
}
