/*
 * Copyright (C)2005-2018 Haxe Foundation
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

// This file is generated from mozilla\MediaKeyStatusMap.webidl. Do not edit!

package js.html.eme;

@:native("MediaKeyStatusMap")
extern class MediaKeyStatusMap
{
	var size(default,null) : Int;
	
	@:overload( function( keyId : js.html.ArrayBuffer) : Bool {} )
	function has( keyId : js.html.ArrayBufferView ) : Bool;
	/** @throws DOMError */
	@:overload( function( keyId : js.html.ArrayBuffer) : Any {} )
	function get( keyId : js.html.ArrayBufferView ) : Any;
	/** @throws DOMError */
	function entries() : js.html.MediaKeyStatusMapIterator;
	/** @throws DOMError */
	function keys() : js.html.MediaKeyStatusMapIterator;
	/** @throws DOMError */
	function values() : js.html.MediaKeyStatusMapIterator;
	/** @throws DOMError */
	function forEach( callback : Any, ?thisArg : Any ) : Void;
}