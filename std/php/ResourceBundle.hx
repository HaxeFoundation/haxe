/*
 * Copyright (C)2005-2021 Haxe Foundation
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

package php;

import haxe.extern.EitherType;

/**
	The `ResourceBundle` class implements access to ICU resource data files.
	@see https://www.php.net/manual/en/class.resourcebundle.php
**/
@:native("ResourceBundle")
extern class ResourceBundle {
	function new(locale: Null<String>, bundle: Null<String>, fallback: Bool = true);

	static function create(locale: Null<String>, bundle: Null<String>, fallback: Bool = true): Null<ResourceBundle>;
	static function getLocales(bundle: String): EitherType<NativeIndexedArray<String>, Bool>;

	function count(): Int;
	function get(index: EitherType<Int, String>, fallback: Bool = true): Dynamic;
	function getErrorCode(): Int;
	function getErrorMessage(): String;
}
