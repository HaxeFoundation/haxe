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

import sys.ssl.Lib;

@:noDoc
typedef CertificatePtr = Dynamic;

@:coreApi
class Certificate {
	var __h:Null<Certificate>;
	var __x:CertificatePtr;

	@:allow(sys.ssl.Socket)
	function new(x:CertificatePtr, ?h:Null<Certificate>) {
		__x = x;
		__h = h;
	}

	public static function loadFile(file:String):Certificate {
		return new Certificate(cert_load_file(haxe.io.Bytes.ofString(file)));
	}

	public static function loadPath(path:String):Certificate {
		return new Certificate(cert_load_path(haxe.io.Bytes.ofString(path)));
	}

	public static function fromString(str:String):Certificate {
		return new Certificate(cert_add_pem(null, haxe.io.Bytes.ofString(str)));
	}

	public static function loadDefaults():Certificate {
		var x = cert_load_defaults();
		if (x != null)
			return new Certificate(x);

		var defPaths = null;
		switch (Sys.systemName()) {
			case "Linux":
				defPaths = [
					"/etc/ssl/certs/ca-certificates.crt", // Debian/Ubuntu/Gentoo etc.
					"/etc/pki/tls/certs/ca-bundle.crt", // Fedora/RHEL
					"/etc/ssl/ca-bundle.pem", // OpenSUSE
					"/etc/pki/tls/cacert.pem", // OpenELEC
					"/etc/ssl/certs", // SLES10/SLES11
					"/system/etc/security/cacerts" // Android
				];
			case "BSD":
				defPaths = [
					"/usr/local/share/certs/ca-root-nss.crt", // FreeBSD/DragonFly
					"/etc/ssl/cert.pem", // OpenBSD
					"/etc/openssl/certs/ca-certificates.crt", // NetBSD
				];
			case "Android":
				defPaths = ["/system/etc/security/cacerts"];
			default:
		}
		if (defPaths != null) {
			for (path in defPaths) {
				if (sys.FileSystem.exists(path)) {
					if (sys.FileSystem.isDirectory(path))
						return loadPath(path);
					else
						return loadFile(path);
				}
			}
		}
		return null;
	}

	public var commonName(get, null):Null<String>;
	public var altNames(get, null):Array<String>;
	public var notBefore(get, null):Date;
	public var notAfter(get, null):Date;

	function get_commonName():Null<String> {
		return subject("CN");
	}

	function get_altNames():Array<String> {
		var a = cert_get_altnames(__x);
		return [for (e in a) e.toString()];
	}

	public function subject(field:String):Null<String> {
		var s = cert_get_subject(__x, haxe.io.Bytes.ofString(field));
		return s == null ? null : s.toString();
	}

	public function issuer(field:String):Null<String> {
		var s = cert_get_issuer(__x, haxe.io.Bytes.ofString(field));
		return s == null ? null : s.toString();
	}

	function get_notBefore():Date {
		var a = cert_get_notbefore(__x);
		return new Date(a[0], a[1] - 1, a[2], a[3], a[4], a[5]);
	}

	function get_notAfter():Date {
		var a = cert_get_notafter(__x);
		return new Date(a[0], a[1] - 1, a[2], a[3], a[4], a[5]);
	}

	public function next():Null<Certificate> {
		var n = cert_get_next(__x);
		return n == null ? null : new Certificate(n, __h == null ? this : __h);
	}

	public function add(pem:String):Void {
		cert_add_pem(__x, haxe.io.Bytes.ofString(pem));
	}

	public function addDER(der:haxe.io.Bytes):Void {
		cert_add_der(__x, der, der.length);
	}

	extern static function cert_load_defaults():CertificatePtr;

	extern static function cert_load_file(file:haxe.io.Bytes):CertificatePtr;

	extern static function cert_load_path(path:haxe.io.Bytes):CertificatePtr;

	extern static function cert_get_subject(cert:CertificatePtr, obj:haxe.io.Bytes):haxe.io.Bytes;

	extern static function cert_get_issuer(cert:CertificatePtr, obj:haxe.io.Bytes):haxe.io.Bytes;

	extern static function cert_get_altnames(cert:CertificatePtr):haxe.ds.Vector<haxe.io.Bytes>;

	extern static function cert_get_notbefore(cert:CertificatePtr):haxe.ds.Vector<Int>;

	extern static function cert_get_notafter(cert:CertificatePtr):haxe.ds.Vector<Int>;

	extern static function cert_get_next(cert:CertificatePtr):Null<CertificatePtr>;

	extern static function cert_add_pem(cert:Null<CertificatePtr>, data:haxe.io.Bytes):CertificatePtr;

	extern static function cert_add_der(cert:Null<CertificatePtr>, data:haxe.io.Bytes, len:Int):CertificatePtr;
}
