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

// This file is generated from mozilla\MediaKeyStatusMap.webidl. Do not edit!

package js.html.eme;

/**
	The `MediaKeyStatusMap` interface of the EncryptedMediaExtensions API is a read-only map of media key statuses by key IDs.

	Documentation [MediaKeyStatusMap](https://developer.mozilla.org/en-US/docs/Web/API/MediaKeyStatusMap) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaKeyStatusMap$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeyStatusMap>
**/
@:native("MediaKeyStatusMap")
extern class MediaKeyStatusMap {
	
	/**
		Returns the number of key/value pars in the status map.
	**/
	var size(default,null) : Int;
	
	
	/**
		Returns a boolean asserting whether a value has been associated with the given key.
	**/
	@:overload( function( keyId : js.lib.ArrayBuffer) : Bool {} )
	function has( keyId : js.lib.ArrayBufferView ) : Bool;
	
	/**
		Returns the value associated with the given key, or `undefined` if there is none.
		@throws DOMError
	**/
	@:overload( function( keyId : js.lib.ArrayBuffer) : Dynamic {} )
	function get( keyId : js.lib.ArrayBufferView ) : Dynamic;
	
	/**
		Returns a new `Iterator` object containing an array of `[key, value]` for each element in the status map, in insertion order.
		@throws DOMError
	**/
	function entries() : js.html.MediaKeyStatusMapIterator;
	
	/**
		Returns a new `Iterator` object containing keys for each element in the status map, in insertion order.
		@throws DOMError
	**/
	function keys() : js.html.MediaKeyStatusMapIterator;
	
	/**
		Returns a new `Iterator` object containing values for each element in the status map, in insertion order.
		@throws DOMError
	**/
	function values() : js.html.MediaKeyStatusMapIterator;
	
	/**
		Calls `callback` once for each key-value pair in the status map, in insertion order. If `argument` is present it will be passed to the callback.
		@throws DOMError
	**/
	function forEach( callback : Dynamic, ?thisArg : Dynamic ) : Void;
}