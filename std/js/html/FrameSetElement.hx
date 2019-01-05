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

// This file is generated from mozilla\HTMLFrameSetElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLFrameSetElement` interface provides special properties (beyond those of the regular `HTMLElement` interface they also inherit) for manipulating `frameset` elements.

	Documentation [HTMLFrameSetElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFrameSetElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFrameSetElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFrameSetElement>
**/
@:native("HTMLFrameSetElement")
extern class FrameSetElement extends Element
{
	
	/**
		Is a `DOMString` structured as a comma-seperated list specifing the width of each column inside a frameset.
	**/
	var cols : String;
	
	/**
		Is a `DOMString` structured as a comma-seperated list specifing the height of each column inside a frameset.
	**/
	var rows : String;
	var onafterprint : haxe.Constraints.Function;
	var onbeforeprint : haxe.Constraints.Function;
	var onbeforeunload : Event -> Null<String>;
	var onhashchange : haxe.Constraints.Function;
	var onlanguagechange : haxe.Constraints.Function;
	var onmessage : haxe.Constraints.Function;
	var onmessageerror : haxe.Constraints.Function;
	var onoffline : haxe.Constraints.Function;
	var ononline : haxe.Constraints.Function;
	var onpagehide : haxe.Constraints.Function;
	var onpageshow : haxe.Constraints.Function;
	var onpopstate : haxe.Constraints.Function;
	var onstorage : haxe.Constraints.Function;
	var onunload : haxe.Constraints.Function;
	
}