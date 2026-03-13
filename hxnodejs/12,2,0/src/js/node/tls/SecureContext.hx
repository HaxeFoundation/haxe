/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.tls;

import haxe.extern.EitherType;

typedef SecureContextOptions = {
	/**
		private key, certificate and CA certs of the server in PFX or PKCS12 format.
	**/
	@:optional var pfx:EitherType<String, Buffer>;

	/**
		passphrase for the private key or pfx.
	**/
	@:optional var passphrase:String;

	/**
		private key of the server in PEM format.
	**/
	@:optional var key:EitherType<String, Buffer>;

	/**
		certificate key of the server in PEM format.
	**/
	@:optional var cert:EitherType<String, Buffer>;

	/**
		trusted certificates in PEM format.
		If this is omitted several well known "root" CAs will be used, like VeriSign.
		These are used to authorize connections.
	**/
	@:optional var ca:Array<EitherType<String, Buffer>>;

	/**
		PEM encoded CRLs (Certificate Revocation List)
	**/
	@:optional var crl:EitherType<String, Array<String>>;

	/**
		ciphers to use or exclude.

		To mitigate BEAST attacks it is recommended that you use this option in conjunction with the `honorCipherOrder`
		option described below to prioritize the non-CBC cipher.

		Defaults to AES128-GCM-SHA256:RC4:HIGH:!MD5:!aNULL:!EDH.

		Consult the OpenSSL cipher list format documentation for details on the format.
		ECDH (Elliptic Curve Diffie-Hellman) ciphers are not yet supported.
	**/
	@:optional var ciphers:String;

	/**
		named curve to use for ECDH key agreement or false to disable ECDH.

		Defaults to prime256v1 (NIST P-256). Use `Crypto.getCurves` to obtain a list of available curve names.
		On recent releases, openssl ecparam -list_curves will also display the name and description
		of each available elliptic curve.
	**/
	@:optional var ecdhCurve:String;

	/**
		Diffie Hellman parameters, required for Perfect Forward Secrecy.

		Use openssl dhparam to create it. Its key length should be greater than or equal to 1024 bits,
		otherwise it throws an error. It is strongly recommended to use 2048 bits or more for stronger security.
		If omitted or invalid, it is silently discarded and DHE ciphers won't be available.
	**/
	@:optional var dhparam:EitherType<String, Buffer>;

	/**
		The SSL method to use, e.g. SSLv3_method to force SSL version 3.
		The possible values depend on your installation of OpenSSL and are defined in the constant SSL_METHODS.
	**/
	@:optional var secureProtocol:String;

	/**
		opaque identifier for session resumption.
		If `requestCert` is true, the default is MD5 hash value generated from command-line.
		Otherwise, the default is not provided.
	**/
	@:optional var sessionIdContext:String;

	/**
		When choosing a cipher, use the server's preferences instead of the client preferences.
		Default: true.
	**/
	@:optional var honorCipherOrder:Bool;
}

extern class SecureContext {}
