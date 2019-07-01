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

@:coreApi
class Digest {
	public static function make(data:haxe.io.Bytes, alg:DigestAlgorithm):haxe.io.Bytes {
		return haxe.io.Bytes.ofData(dgst_make(data.getData(), untyped alg.__s));
	}

	public static function sign(data:haxe.io.Bytes, privKey:Key, alg:DigestAlgorithm):haxe.io.Bytes {
		return haxe.io.Bytes.ofData(dgst_sign(data.getData(), @:privateAccess privKey.__k, untyped alg.__s));
	}

	public static function verify(data:haxe.io.Bytes, signature:haxe.io.Bytes, pubKey:Key, alg:DigestAlgorithm):Bool {
		return dgst_verify(data.getData(), signature.getData(), @:privateAccess pubKey.__k, untyped alg.__s);
	}

	private static var dgst_make = neko.Lib.loadLazy("ssl", "dgst_make", 2);
	private static var dgst_sign = neko.Lib.loadLazy("ssl", "dgst_sign", 3);
	private static var dgst_verify = neko.Lib.loadLazy("ssl", "dgst_verify", 4);
}
