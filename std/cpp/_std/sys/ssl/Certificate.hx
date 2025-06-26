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

import cpp.NativeSsl;

@:coreApi
class Certificate {
	var __h:Null<Certificate>;
	var __x:Dynamic;

	@:allow(sys.ssl.Socket)
	function new(x:Dynamic, ?h:Certificate) {
		__x = x;
		__h = h;
	}

	public static function loadFile(file:String):Certificate {
		return new Certificate(NativeSsl.cert_load_file(file));
	}

	public static function loadPath(path:String):Certificate {
		return new Certificate(NativeSsl.cert_load_path(path));
	}

	public static function fromString(str:String):Certificate {
		return new Certificate(NativeSsl.cert_add_pem(null, str));
	}

	public static function loadDefaults():Certificate {
		var x = NativeSsl.cert_load_defaults();
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
		return NativeSsl.cert_get_altnames(__x);
	}

	public function subject(field:String):Null<String> {
		return NativeSsl.cert_get_subject(__x, field);
	}

	public function issuer(field:String):Null<String> {
		return NativeSsl.cert_get_issuer(__x, field);
	}

	function get_notBefore():Date {
		var a = NativeSsl.cert_get_notbefore(__x);
		return new Date(a[0], a[1] - 1, a[2], a[3], a[4], a[5]);
	}

	function get_notAfter():Date {
		var a = NativeSsl.cert_get_notafter(__x);
		return new Date(a[0], a[1] - 1, a[2], a[3], a[4], a[5]);
	}

	public function next():Null<Certificate> {
		var n = NativeSsl.cert_get_next(__x);
		return n == null ? null : new Certificate(n, __h == null ? this : __h);
	}

	public function add(pem:String):Void {
		NativeSsl.cert_add_pem(__x, pem);
	}

	public function addDER(der:haxe.io.Bytes):Void {
		NativeSsl.cert_add_der(__x, der.getData());
	}

	static function __init__():Void {
		NativeSsl.init();
	}
}
