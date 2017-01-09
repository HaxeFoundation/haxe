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

// This file is generated from mozilla\HTMLScriptElement.webidl. Do not edit!

package js.html;

/**
	HTML script elements expose the `HTMLScriptElement` interface, which provides special properties and methods (beyond the regular `HTMLElement` object interface they also have available to them by inheritance) for manipulating the layout and presentation of `script` elements.

	Documentation [HTMLScriptElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLScriptElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLScriptElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLScriptElement>
**/
@:native("HTMLScriptElement")
extern class ScriptElement extends Element
{
	var src : String;
	var type : String;
	var charset : String;
	var async : Bool;
	var defer : Bool;
	var crossOrigin : String;
	var text : String;
	var event : String;
	var htmlFor : String;
	var integrity : String;
	
}