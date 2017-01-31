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

// This file is generated from mozilla\SVGTextContentElement.webidl. Do not edit!

package js.html.svg;

@:native("SVGTextContentElement")
extern class TextContentElement extends GraphicsElement
{
	static inline var LENGTHADJUST_UNKNOWN : Int = 0;
	static inline var LENGTHADJUST_SPACING : Int = 1;
	static inline var LENGTHADJUST_SPACINGANDGLYPHS : Int = 2;
	
	var textLength(default,null) : AnimatedLength;
	var lengthAdjust(default,null) : AnimatedEnumeration;
	
	function getNumberOfChars() : Int;
	function getComputedTextLength() : Float;
	/** @throws DOMError */
	function getSubStringLength( charnum : Int, nchars : Int ) : Float;
	/** @throws DOMError */
	function getStartPositionOfChar( charnum : Int ) : Point;
	/** @throws DOMError */
	function getEndPositionOfChar( charnum : Int ) : Point;
	/** @throws DOMError */
	function getExtentOfChar( charnum : Int ) : Rect;
	/** @throws DOMError */
	function getRotationOfChar( charnum : Int ) : Float;
	function getCharNumAtPosition( point : Point ) : Int;
	/** @throws DOMError */
	function selectSubString( charnum : Int, nchars : Int ) : Void;
}