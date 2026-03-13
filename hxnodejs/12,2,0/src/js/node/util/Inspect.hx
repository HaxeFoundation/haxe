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

package js.node.util;

import haxe.DynamicAccess;
import js.node.Util.InspectOptions;

@:jsRequire("util", "inspect")
extern class Inspect {
	/**
		The `util.inspect()` method returns a string representation of `object` that is intended for debugging.

		@see https://nodejs.org/api/util.html#util_util_inspect_object_options
	**/
	@:selfCall
	@:overload(function(object:Dynamic, ?showHidden:Bool, ?depth:Int, ?colors:Bool):String {})
	static function inspect(object:Dynamic, ?options:InspectOptions):String;

	/**
		`util.inspect.styles` is a map associating a style name to a color from `util.inspect.colors` properties.

		@see https://nodejs.org/api/util.html#util_customizing_util_inspect_colors
	**/
	static var styles:DynamicAccess<String>;

	/**
		The predefined color codes are: `white`, `grey`, `black`, `blue`, `cyan`, `green`, `magenta`, `red` and
		`yellow`.
	**/
	static var colors:DynamicAccess<Array<Int>>;

	/**
		In addition to being accessible through `util.inspect.custom`, this symbol is registered globally and can be
		accessed in any environment as `Symbol.for('nodejs.util.inspect.custom')`.

		@see https://nodejs.org/api/util.html#util_util_inspect_custom
	**/
	#if haxe4
	static final custom:js.lib.Symbol;
	#else
	static var custom(default, never):Dynamic;
	#end

	/**
		The `defaultOptions` value allows customization of the default options used by `util.inspect`.

		@see https://nodejs.org/api/util.html#util_util_inspect_defaultoptions
	**/
	static var defaultOptions:InspectOptions;
}
