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

package cpp.uv;

using cpp.uv.UV;

/**
	Version information for the vendored libuv.
	@see http://docs.libuv.org/en/v1.x/version.html
**/
@:headerCode('#include "uv.h"')
class Version {
	/**
		Returns the libuv version as a string.
	**/
	static public function string():String {
		return UV.version_string().toString();
	}

	/**
		libuv major version number.
	**/
	static public var major(get,never):Int;
	static function get_major():Int return untyped __cpp__('UV_VERSION_MAJOR');

	/**
		libuv minor version number.
	**/
	static public var minor(get,never):Int;
	static function get_minor():Int return untyped __cpp__('UV_VERSION_MINOR');

	/**
		libuv patch version number.
	**/
	static public var patch(get,never):Int;
	static function get_patch():Int return untyped __cpp__('UV_VERSION_PATCH');

	/**
		`true` if the libuv version is a release, and `false` if it is a development version.
		This does not depend on Haxe compilation arguments and will almost always be `true`.
	**/
	static public var isRelease(get,never):Bool;
	static function get_isRelease():Bool return 0 != untyped __cpp__('UV_VERSION_IS_RELEASE');

	/**
		libuv version suffix for development releases.
	**/
	static public var suffix(get,never):String;
	static function get_suffix():String return (untyped __cpp__('UV_VERSION_SUFFIX'):ConstCharStar).toString();

	/**
		Returns the libuv version packed into a single integer.
		8 bits are used for each component, with the patch number stored in the 8
		least significant bits. E.g. for libuv 1.2.3 this would be 0x010203.
	**/
	static public var hex(get,never):Int;
	static function get_hex():Int return untyped __cpp__('UV_VERSION_HEX');
}