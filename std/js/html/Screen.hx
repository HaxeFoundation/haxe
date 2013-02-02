/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** Returns a reference to the screen object associated with the window.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/window.screen">MDN</a>. */
@:native("Screen")
extern class Screen
{
	/** Specifies the height of the screen, in pixels, minus permanent or semipermanent user interface features displayed by the operating system, such as the Taskbar on Windows. */
	var availHeight (default,null) : Int;

	/** Returns the first available pixel available from the left side of the screen. */
	var availLeft (default,null) : Int;

	/** Specifies the y-coordinate of the first pixel that is not allocated to permanent or semipermanent user interface features. */
	var availTop (default,null) : Int;

	/** Returns the amount of horizontal space in pixels available to the window. */
	var availWidth (default,null) : Int;

	/** Returns the color depth of the screen. */
	var colorDepth (default,null) : Int;

	/** Returns the height of the screen in pixels. */
	var height (default,null) : Int;

	/** Gets the bit depth of the screen. */
	var pixelDepth (default,null) : Int;

	/** Returns the width of the screen. */
	var width (default,null) : Int;

}
