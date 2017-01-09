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

// This file is generated from mozilla\XMLHttpRequestEventTarget.webidl. Do not edit!

package js.html;

/**
	`XMLHttpRequestEventTarget` is the interface that describes the event handlers you can implement in an object that will handle events for an `XMLHttpRequest`.

	Documentation [XMLHttpRequestEventTarget](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget>
**/
@:native("XMLHttpRequestEventTarget")
extern class XMLHttpRequestEventTarget extends EventTarget
{
	var onloadstart : haxe.Constraints.Function;
	var onprogress : haxe.Constraints.Function;
	var onabort : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	var onload : haxe.Constraints.Function;
	var ontimeout : haxe.Constraints.Function;
	var onloadend : haxe.Constraints.Function;
	
}