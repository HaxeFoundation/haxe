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

// This file is generated from mozilla\ScreenOrientation.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `ScreenOrientation` interface of the the Screen Orientation API provides information about the current orientation of the document.

	Documentation [ScreenOrientation](https://developer.mozilla.org/en-US/docs/Web/API/ScreenOrientation) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ScreenOrientation$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ScreenOrientation>
**/
@:native("ScreenOrientation")
extern class ScreenOrientation extends EventTarget {

	/**
		Returns the document's current orientation type, one of "portrait-primary", "portrait-secondary", "landscape-primary", or "landscape-secondary".
	**/
	var type(default,null) : OrientationType;

	/**
		Returns the document's current orientation angle.
	**/
	var angle(default,null) : Int;

	/**
		Fired whenever is the `EventHandler` called when the screen changes orientation.
	**/
	var onchange : haxe.Constraints.Function;


	/**
		Locks the orientation of the containing document to its default orientation and returns a `Promise`. 
		@throws DOMError
	**/
	function lock( orientation : OrientationLockType ) : Promise<Void>;

	/**
		Unlocks the orientation of the containing document from its default orientation.
		@throws DOMError
	**/
	function unlock() : Void;
}