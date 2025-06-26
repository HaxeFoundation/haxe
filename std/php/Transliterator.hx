/*
 * Copyright (C)2005-2020 Haxe Foundation
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
	@see https://www.php.net/manual/en/class.transliterator.php
**/
@:native("Transliterator")
extern class Transliterator {
	@:phpClassConst static final FORWARD: Int;
	@:phpClassConst static final REVERSE: Int;

	var id: String;

	static function create(id: String, ?direction: Int): Null<Transliterator>;
	static function createFromRules(rules: String, ?direction: Int): Null<Transliterator>;
	static function listIDs(): EitherType<Array<String>, Bool>;

	function createInverse(): Null<Transliterator>;
	function getErrorCode(): EitherType<Int, Bool>;
	function getErrorMessage(): EitherType<String, Bool>;
	function transliterate(subject: String, ?start: Int, ?end: Int): EitherType<String, Bool>;
}
