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

import haxe.io.Bytes;
import sys.ssl.Mbedtls;
import mbedtls.X509Crt;

@:coreApi
class Certificate {
	var native:X509Crt;

	function new(native:X509Crt) {
		this.native = native;
	}

	public static function loadFile(file:String):Certificate {
		var cert = new X509Crt();
		cert.parse_file(file);
		return new Certificate(cert);
	}

	public static function loadPath(path:String):Certificate {
		var cert = new X509Crt();
		cert.parse_path(path);
		return new Certificate(cert);
	}

	public static function fromString(str:String):Certificate {
		var cert = new X509Crt();
		trace(mbedtls.Error.strerror(cert.parse(Bytes.ofString(str))));
		return new Certificate(cert);
	}

	public static function loadDefaults():Certificate {
		var cert = new X509Crt();
		Mbedtls.loadDefaultCertificates(cert);
		return new Certificate(cert);
	}

	public var commonName(get, null):Null<String>;

	public var altNames(get, null):Array<String>;

	public var notBefore(get, null):Date;

	public var notAfter(get, null):Date;

	extern public function subject(field:String):Null<String>;

	extern public function issuer(field:String):Null<String>;

	public function next():Null<Certificate> {
		var cert = native.next();
		if (cert == null) {
			return null;
		}
		return new Certificate(cert);
	}

	public function add(pem:String):Void {
		native.parse(Bytes.ofString(pem));
	}

	public function addDER(der:Bytes):Void {
		native.parse(der);
	}

	private function get_commonName():Null<String> {
		return subject("CN");
	}

	extern private function get_altNames():Array<String>;

	extern private function get_notBefore():Date;

	extern private function get_notAfter():Date;

	private inline function getNative():X509Crt {
		return native;
	}
}
