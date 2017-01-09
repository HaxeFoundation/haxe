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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl;

/**
	The WebGLActiveInfo interface is part of the WebGL API and represents the information returned by calling the `WebGLRenderingContext.getActiveAttrib()` and `WebGLRenderingContext.getActiveUniform()` methods.

	Documentation [WebGLActiveInfo](https://developer.mozilla.org/en-US/docs/Web/API/WebGLActiveInfo) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WebGLActiveInfo$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WebGLActiveInfo>
**/
@:native("WebGLActiveInfo")
extern class ActiveInfo
{
	
	/**
		The read-only size of the requested variable.
	**/
	var size(default,null) : Int;
	
	/**
		The read-only type of the requested variable.
	**/
	var type(default,null) : Int;
	
	/**
		The read-only name of the requested variable.
	**/
	var name(default,null) : String;
	
}