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

// This file is generated from mozilla\NodeFilter.webidl. Do not edit!

package js.html;

@:native("NodeFilter")
extern class NodeFilter
{
	static inline var FILTER_ACCEPT : Int = 1;
	static inline var FILTER_REJECT : Int = 2;
	static inline var FILTER_SKIP : Int = 3;
	static inline var SHOW_ALL : Int = cast 4294967295;
	static inline var SHOW_ELEMENT : Int = 1;
	static inline var SHOW_ATTRIBUTE : Int = 2;
	static inline var SHOW_TEXT : Int = 4;
	static inline var SHOW_CDATA_SECTION : Int = 8;
	static inline var SHOW_ENTITY_REFERENCE : Int = 16;
	static inline var SHOW_ENTITY : Int = 32;
	static inline var SHOW_PROCESSING_INSTRUCTION : Int = 64;
	static inline var SHOW_COMMENT : Int = 128;
	static inline var SHOW_DOCUMENT : Int = 256;
	static inline var SHOW_DOCUMENT_TYPE : Int = 512;
	static inline var SHOW_DOCUMENT_FRAGMENT : Int = 1024;
	static inline var SHOW_NOTATION : Int = 2048;
	
	function acceptNode( node : Node ) : Int;
}