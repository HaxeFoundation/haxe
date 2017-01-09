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

// This file is generated from mozilla\Screen.webidl. Do not edit!

package js.html;

/**
	The `Screen` interface represents a screen, usually the one on which the current window is being rendered.

	Documentation [Screen](https://developer.mozilla.org/en-US/docs/Web/API/Screen) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Screen$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Screen>
**/
@:native("Screen")
extern class Screen extends EventTarget
{
	var availWidth(default,null) : Int;
	var availHeight(default,null) : Int;
	var width(default,null) : Int;
	var height(default,null) : Int;
	var colorDepth(default,null) : Int;
	var pixelDepth(default,null) : Int;
	var top(default,null) : Int;
	var left(default,null) : Int;
	var availTop(default,null) : Int;
	var availLeft(default,null) : Int;
	var orientation(default,null) : ScreenOrientation;
	
}