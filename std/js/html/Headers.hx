/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\Headers.webidl line 26:0. Do not edit!

package js.html;

@:native("Headers")
extern class Headers
{
	/** @throws DOMError */
	function new( ?init : haxe.extern.EitherType<Headers,haxe.extern.EitherType<Array<Array<String>>,Dynamic/*MISSING ByteStringMozMap*/>> ) : Void;
	/** @throws DOMError */
	function append( name : String, value : String ) : Void;
	/** @throws DOMError */
	@:native("delete")
	function delete_( name : String ) : Void;
	/** @throws DOMError */
	function get( name : String ) : String;
	/** @throws DOMError */
	function getAll( name : String ) : Array<String>;
	/** @throws DOMError */
	function has( name : String ) : Bool;
	/** @throws DOMError */
	function set( name : String, value : String ) : Void;
	/** @throws DOMError */
	function entries() : HeadersIterator;
	/** @throws DOMError */
	function keys() : HeadersIterator;
	/** @throws DOMError */
	function values() : HeadersIterator;
	/** @throws DOMError */
	function forEach( callback : Dynamic, ?thisArg : Dynamic ) : Void;
}