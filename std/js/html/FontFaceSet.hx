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

// This file is generated from mozilla\FontFaceSet.webidl. Do not edit!

package js.html;

@:native("FontFaceSet")
extern class FontFaceSet extends EventTarget
{
	var size(default,null) : Int;
	var onloading : haxe.Constraints.Function;
	var onloadingdone : haxe.Constraints.Function;
	var onloadingerror : haxe.Constraints.Function;
	var ready(default,null) : Promise<Void>;
	var status(default,null) : FontFaceSetLoadStatus;
	
	/** @throws DOMError */
	function add( font : FontFace ) : Void;
	function has( font : FontFace ) : Bool;
	@:native("delete")
	function delete_( font : FontFace ) : Bool;
	function clear() : Void;
	function entries() : FontFaceSetIterator;
	function values() : FontFaceSetIterator;
	/** @throws DOMError */
	function forEach( cb : FontFace -> FontFace -> FontFaceSet -> Void, ?thisArg : Dynamic ) : Void;
	function load( font : String, ?text : String = " " ) : Promise<Array<FontFace>>;
	/** @throws DOMError */
	function check( font : String, ?text : String = " " ) : Bool;
}