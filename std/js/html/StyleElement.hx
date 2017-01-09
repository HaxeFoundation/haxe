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

// This file is generated from mozilla\HTMLStyleElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLStyleElement` interface represents a `style` element. It inherits properties and methods from its parent, `HTMLElement`, and from `LinkStyle`.

	Documentation [HTMLStyleElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLStyleElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLStyleElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLStyleElement>
**/
@:native("HTMLStyleElement")
extern class StyleElement extends Element
{
	
	/**
		Is a `Boolean` value representing whether or not the stylesheet is disabled (true) or not (false).
	**/
	var disabled : Bool;
	
	/**
		Is a `DOMString` representing the intended destination medium for style information.
	**/
	var media : String;
	
	/**
		Is a `DOMString` representing the type of style being applied by this statement.
	**/
	var type : String;
	
	/**
		Is a `Boolean` value indicating if the element applies to the whole document (`false`) or only to the parent's sub-tree (`true`).
	**/
	var scoped : Bool;
	var sheet(default,null) : StyleSheet;
	
}