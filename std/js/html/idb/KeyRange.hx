/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\IDBKeyRange.webidl. Do not edit!

package js.html.idb;

/**
	A key range can be a single value or a range with upper and lower bounds or endpoints. If the key range has both upper and lower bounds, then it is bounded; if it has no bounds, it is unbounded. A bounded key range can either be open (the endpoints are excluded) or closed (the endpoints are included). To retrieve all keys within a certain range, you can use the following code constructs:

	Documentation [IDBKeyRange](https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange>
**/
@:native("IDBKeyRange")
extern class KeyRange
{
	/** @throws DOMError */
	static 
	/**
		Creates a new key range containing a single value.
	**/
	function only( value : Dynamic ) : KeyRange;
	/** @throws DOMError */
	static 
	/**
		Creates a new key range with only a lower bound.
	**/
	function lowerBound( lower : Dynamic, ?open : Bool = false ) : KeyRange;
	/** @throws DOMError */
	static 
	/**
		Creates a new upper-bound key range.
	**/
	function upperBound( upper : Dynamic, ?open : Bool = false ) : KeyRange;
	/** @throws DOMError */
	static 
	/**
		Creates a new key range with upper and lower bounds.
	**/
	function bound( lower : Dynamic, upper : Dynamic, ?lowerOpen : Bool = false, ?upperOpen : Bool = false ) : KeyRange;
	
	/**
		Lower bound of the key range.
	**/
	var lower(default,null) : Dynamic;
	
	/**
		Upper bound of the key range.
	**/
	var upper(default,null) : Dynamic;
	
	/**
		Returns false if the lower-bound value is included in the key range.
	**/
	var lowerOpen(default,null) : Bool;
	
	/**
		Returns false if the upper-bound value is included in the key range.
	**/
	var upperOpen(default,null) : Bool;
	
	/** @throws DOMError */
	
	/**
		Returns a boolean indicating whether a specified key is inside the key range.
	**/
	function includes( key : Dynamic ) : Bool;
}