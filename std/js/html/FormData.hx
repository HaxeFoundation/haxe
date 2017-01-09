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

// This file is generated from mozilla\FormData.webidl. Do not edit!

package js.html;

/**
	The `FormData` interface provides a way to easily construct a set of key/value pairs representing form fields and their values, which can then be easily sent using the `XMLHttpRequest.send()` method. It uses the same format a form would use if the encoding type were set to `"multipart/form-data"`.

	Documentation [FormData](https://developer.mozilla.org/en-US/docs/Web/API/FormData) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FormData$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FormData>
**/
@:native("FormData")
extern class FormData
{
	/** @throws DOMError */
	function new( ?form : FormElement ) : Void;
	/** @throws DOMError */
	@:overload( function( name : String, value : Blob, ?filename : String ) : Void {} )
	
	/**
		Appends a new value onto an existing key inside a `FormData` object, or adds the key if it does not already exist.
	**/
	function append( name : String, value : String ) : Void;
	@:native("delete")
	function delete_( name : String ) : Void;
	
	/**
		Returns the first value associated with a given key from within a `FormData` object.
	**/
	function get( name : String ) : haxe.extern.EitherType<Blob,String>;
	
	/**
		Returns an array of all the values associated with a given key from within a `FormData`.
	**/
	function getAll( name : String ) : Array<haxe.extern.EitherType<Blob,String>>;
	
	/**
		Returns a boolean stating whether a `FormData` object contains a certain key/value pair.
	**/
	function has( name : String ) : Bool;
	/** @throws DOMError */
	@:overload( function( name : String, value : Blob, ?filename : String ) : Void {} )
	
	/**
		Sets a new value for an existing key inside a `FormData `object, or adds the key/value if it does not already exist.
	**/
	function set( name : String, value : String ) : Void;
	/** @throws DOMError */
	
	/**
		Returns an `Iteration_protocols` allowing to go through all key/value pairs contained in this object.
	**/
	function entries() : FormDataIterator;
	/** @throws DOMError */
	
	/**
		Returns an `Iteration_protocols` allowing to go through all keys of the key/value pairs contained in this object.
	**/
	function keys() : FormDataIterator;
	/** @throws DOMError */
	
	/**
		Returns an `Iteration_protocols` allowing to go through all values of the key/value pairs contained in this object.
	**/
	function values() : FormDataIterator;
	/** @throws DOMError */
	function forEach( callback : Dynamic, ?thisArg : Dynamic ) : Void;
}