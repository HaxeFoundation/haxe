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

package js.node;

@:jsRequire("constants")
extern class Constants {
	static var ENGINE_METHOD_RSA(default, null):Int;
	static var ENGINE_METHOD_DSA(default, null):Int;
	static var ENGINE_METHOD_DH(default, null):Int;
	static var ENGINE_METHOD_RAND(default, null):Int;
	static var ENGINE_METHOD_ECDH(default, null):Int;
	static var ENGINE_METHOD_ECDSA(default, null):Int;
	static var ENGINE_METHOD_CIPHERS(default, null):Int;
	static var ENGINE_METHOD_DIGESTS(default, null):Int;
	static var ENGINE_METHOD_STORE(default, null):Int;
	static var ENGINE_METHOD_PKEY_METH(default, null):Int;
	static var ENGINE_METHOD_PKEY_ASN1_METH(default, null):Int;
	static var ENGINE_METHOD_ALL(default, null):Int;
	static var ENGINE_METHOD_NONE(default, null):Int;

	static var RSA_NO_PADDING(default, null):Int;
	static var RSA_PKCS1_PADDING(default, null):Int;
	static var RSA_PKCS1_OAEP_PADDING(default, null):Int;
}
