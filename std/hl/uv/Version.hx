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

package hl.uv;

/**
	Version information for the vendored libuv.

	@see http://docs.libuv.org/en/v1.x/version.html
**/
class Version {
	/**
		Returns the libuv version as a string.
	**/
	static public inline function string():String
		return @:privateAccess String.fromUTF8(stringWrap());

	@:hlNative("uv","version_string_wrap")
	static function stringWrap():Bytes
		return null;

	/**
		libuv major version number.
	**/
	static public var major(get,never):Int;
	@:hlNative("uv","version_major")
	static function get_major():Int return 0;

	/**
		libuv minor version number.
	**/
	static public var minor(get,never):Int;
	@:hlNative("uv","version_minor")
	static function get_minor():Int return 0;

	/**
		libuv patch version number.
	**/
	static public var patch(get,never):Int;
	@:hlNative("uv","version_patch")
	static function get_patch():Int return 0;

	/**
		`true` if the libuv version is a release, and `false` if it is a development version.
		This does not depend on Haxe compilation arguments and will almost always be `true`.
	**/
	static public var isRelease(get,never):Bool;
	@:hlNative("uv","version_is_release")
	static function get_isRelease():Bool return false;

	/**
		libuv version suffix for development releases.
	**/
	static public var suffix(get,never):String;
	static inline function get_suffix():String return @:privateAccess String.fromUTF8(get_suffixWrap());
	@:hlNative("uv","version_suffix")
	static function get_suffixWrap():Bytes return null;

	/**
		Returns the libuv version packed into a single integer.

		8 bits are used for each component, with the patch number stored in the 8
		least significant bits. E.g. for libuv 1.2.3 this would be 0x010203.
	**/
	static public var hex(get,never):Int;
	@:hlNative("uv","version_hex")
	static function get_hex():Int return 0;
}
