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

// This file is generated from mozilla\Text.webidl. Do not edit!

package js.html;

/**
	The `Text` interface represents the textual content of `Element` or `Attr`.  If an element has no markup within its content, it has a single child implementing `Text` that contains the element's text.  However, if the element contains markup, it is parsed into information items and `Text` nodes that form its children.

	Documentation [Text](https://developer.mozilla.org/en-US/docs/Web/API/Text) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Text$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Text>
**/
@:native("Text")
extern class Text extends CharacterData
{
	
	/**
		Returns a `DOMString` containing the text of all `Text` nodes logically adjacent to this `Node`, concatenated in document order.
	**/
	var wholeText(default,null) : String;
	
	/** @throws DOMError */
	function new( ?data : String = "" ) : Void;
	/** @throws DOMError */
	function splitText( offset : Int ) : Text;
	/** @throws DOMError */
	function convertQuadFromNode( quad : DOMQuad, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertRectFromNode( rect : DOMRectReadOnly, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertPointFromNode( point : DOMPointInit, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMPoint;
}