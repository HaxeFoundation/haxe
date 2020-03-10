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

package python.lib;

import python.lib.ssl.SSLContext;

@:pythonImport("ssl")
extern class Ssl {
	@:require(python_version >= 3.4)
	static function create_default_context(purpose:String):SSLContext;

	/**
		Prevents a TLSv1 connection. This option is only applicable in conjunction
		with PROTOCOL_TLS. It prevents the peers from choosing TLSv1 as the
		protocol version.
	**/
	static var OP_NO_TLSv1:Int;

	/**
		Prevents a TLSv1.1 connection. This option is only applicable in conjunction
		with PROTOCOL_TLS. It prevents the peers from choosing TLSv1.1 as the
		protocol version. Available only with openssl version 1.0.1+.

		since python 3.4
	**/
	@:require(python_version >= 3.4)
	static var OP_NO_TLSv1_1:Int;

	static var OP_NO_SSLv3:Int;
	static var OP_NO_SSLv2:Int;

	static var OP_NO_COMPRESSION:Int;

	#if (python_version >= 3.6)
	@:deprecated("deprecated, use PROTOCOL_TLS instead")
	#end
	static var PROTOCOL_SSLv23:String;

	@:require(python_version >= 3.6)
	static var PROTOCOL_TLS:String;

	static var CERT_REQUIRED:Int;
}
