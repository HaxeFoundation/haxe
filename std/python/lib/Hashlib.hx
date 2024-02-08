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

import python.Bytes;
import python.Set;

@:pythonImport("hashlib")
extern class Hashlib {
	/**
		A set containing the names of the hash algorithms guaranteed to be supported by this module on all platforms.
		Note that ‘md5’ is in this list despite some upstream vendors offering an odd “FIPS compliant” Python build that excludes it.
	**/
	static var algorithms_guaranteed(default, null):Set<String>;

	/**
		A set containing the names of the hash algorithms that are available in the running Python interpreter.
		These names will be recognized when passed to new(). algorithms_guaranteed will always be a subset.
		The same algorithm may appear multiple times in this set under different names (thanks to OpenSSL).
	**/
	static var algorithms_available(default, null):Set<String>;

	/**
		A generic constructor that takes the string name of the desired algorithm as its first parameter.
		It also exists to allow access to the above listed hashes as well as any other algorithms that your OpenSSL library may offer.
		The named constructors are much faster than new() and should be preferred.
	**/
	@:native("new")
	static function new_(name:String, ?data:Bytes):HashlibHash;

	static function sha1():HashlibHash;
	static function sha224():HashlibHash;
	static function sha256():HashlibHash;
	static function sha384():HashlibHash;
	static function sha512():HashlibHash;
	static function blake2b():HashlibHash;
	static function blake2s():HashlibHash;
	static function md5():HashlibHash;
	static function sha3_224():HashlibHash;
	static function sha3_256():HashlibHash;
	static function sha3_384():HashlibHash;
	static function sha3_512():HashlibHash;
	static function shake_128():HashlibHash;
	static function shake_256():HashlibHash;
}

extern class HashlibHash {
	/**
		The size of the resulting hash in bytes.
	**/
	var digest_size(default, null):Int;

	/**
		The internal block size of the hash algorithm in bytes.
	**/
	var block_size(default, null):Int;

	/**
		The canonical name of this hash, always lowercase and always suitable as a parameter to new() to create another hash of this type.
	**/
	var name:String;

	/**
		Update the hash object with the object arg, which must be interpretable as a buffer of bytes.
		Repeated calls are equivalent to a single call with the concatenation of all the arguments: m.update(a); m.update(b) is equivalent to m.update(a+b).
	**/
	function update(a:Bytes):Void;

	/**
		Return the digest of the data passed to the update() method so far.
		This is a bytes object of size digest_size which may contain bytes in the whole range from 0 to 255.
	**/
	function digest():Bytes;

	/**
		Like digest() except the digest is returned as a string object of double length, containing only hexadecimal digits.
		This may be used to exchange the value safely in email or other non-binary environments.
	**/
	function hexdigest():String;

	/**
		Return a copy (“clone”) of the hash object.
		This can be used to efficiently compute the digests of data sharing a common initial substring.
	**/
	function copy():HashlibHash;
}
