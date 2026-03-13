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

package js.node.crypto;

import js.node.Buffer;

/**
	SPKAC is a Certificate Signing Request mechanism originally implemented by Netscape
	and now specified formally as part of HTML5's keygen element.
**/
@:jsRequire("crypto", "Certificate")
extern class Certificate {
	function new();

	/**
		Returns the challenge component in the form of a Node.js `Buffer`.

		The `spkac` data structure includes a public key and a challenge.
	**/
	@:overload(function(spkac:String):Buffer {})
	function exportChallenge(spkac:Buffer):Buffer;

	/**
		Returns the public key component in the form of a Node.js `Buffer`.

		The `spkac` data structure includes a public key and a challenge.
	**/
	@:overload(function(spkac:String):Buffer {})
	function exportPublicKey(spkac:Buffer):Buffer;

	/**
		Returns true if the given `spkac` data structure is valid, false otherwise.
	**/
	function verifySpkac(spkac:Buffer):Bool;
}
