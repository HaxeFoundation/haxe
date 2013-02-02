/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.idb;

/** The <code>IDBKeyRange</code> interface of the <a title="en/IndexedDB" rel="internal" href="https://developer.mozilla.org/en/IndexedDB">IndexedDB API</a> represents a continuous interval over some data type that is used for keys. Records can be retrieved from object stores and indexes using keys or a range of keys. You can limit the range using lower and upper bounds. For example, you can iterate over all values of a key between x and y.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/IndexedDB/IDBKeyRange">MDN</a>. */
@:native("IDBKeyRange")
extern class KeyRange
{
	var lower (default,null) : Key;

	var lowerOpen (default,null) : Bool;

	var upper (default,null) : Key;

	/** Returns false if the upper-bound value is included in the key range. */
	var upperOpen (default,null) : Bool;

	static function bound( lower : Key, upper : Key, ?lowerOpen : Bool, ?upperOpen : Bool ) : KeyRange;

	static function lowerBound( bound : Key, ?open : Bool ) : KeyRange;

	static function only( value : Key ) : KeyRange;

	static function upperBound( bound : Key, ?open : Bool ) : KeyRange;

}
