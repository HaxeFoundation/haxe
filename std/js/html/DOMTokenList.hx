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

// This file is generated from mozilla\DOMTokenList.webidl. Do not edit!

package js.html;

/**
	The `DOMTokenList` interface represents a set of space-separated tokens. Such a set is returned by `Element.classList`, `HTMLLinkElement.relList`, `HTMLAnchorElement.relList`, `HTMLAreaElement.relList`, `HTMLIframeElement.sandbox`, or `HTMLOutputElement.htmlFor`. It is indexed beginning with `0` as with JavaScript `Array` objects. `DOMTokenList` is always case-sensitive.

	Documentation [DOMTokenList](https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList>
**/
@:native("DOMTokenList")
extern class DOMTokenList implements ArrayAccess<String> {
	
	/**
		Is an `integer` representing the number of objects stored in the object.
	**/
	var length(default,null) : Int;
	
	/**
		The value of the list as a `DOMString`.
	**/
	var value : String;
	
	
	/**
		Returns an item in the list by its index (returns undefined if the number is greater than or equal to the length of the list).
	**/
	function item( index : Int ) : String;
	
	/**
		Returns `true` if the list contains the given token, otherwise `false`.
	**/
	function contains( token : String ) : Bool;
	
	/**
		Adds the given token to the list.
		@throws DOMError
	**/
	function add( tokens : haxe.extern.Rest<String> ) : Void;
	
	/**
		Removes the specified token from the list.
		@throws DOMError
	**/
	function remove( tokens : haxe.extern.Rest<String> ) : Void;
	
	/**
		Replaces an existing token with a new token.
		@throws DOMError
	**/
	function replace( token : String, newToken : String ) : Bool;
	
	/**
		Removes a given token from the list and returns false. If token doesn't exist it's added and the function returns `true`.
		@throws DOMError
	**/
	function toggle( token : String, ?force : Bool ) : Bool;
	
	/**
		Returns `true` if a given token is in the associated attribute's supported tokens.
		@throws DOMError
	**/
	function supports( token : String ) : Bool;
}