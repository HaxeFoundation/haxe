/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/FileReader.webidl line 14:0. Do not edit!

package js.html;

@:native("FileReader")
extern class FileReader extends EventTarget
{
	static inline var EMPTY : Int = 0;
	static inline var LOADING : Int = 1;
	static inline var DONE : Int = 2;
	
	var readyState(default,null) : Int;
	var result(default,null) : Dynamic;
	var error(default,null) : DOMError;
	var onloadstart : haxe.Constraints.Function;
	var onprogress : haxe.Constraints.Function;
	var onload : haxe.Constraints.Function;
	var onabort : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onloadend : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	/** @throws DOMError */
	function readAsArrayBuffer( blob : Blob ) : Void;
	/** @throws DOMError */
	function readAsText( blob : Blob, ?label : String = "" ) : Void;
	/** @throws DOMError */
	function readAsDataURL( blob : Blob ) : Void;
	/** @throws DOMError */
	function abort() : Void;
	/** @throws DOMError */
	function readAsBinaryString( filedata : Blob ) : Void;
}