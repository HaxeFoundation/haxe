/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/HTMLCanvasElement.webidl line 17:0. Do not edit!

package js.html;

@:native("HTMLCanvasElement")
extern class CanvasElement extends Element
{
	var width : Int;
	var height : Int;
	
	/** @throws DOMError */
	function getContext( contextId : String, ?contextOptions : Dynamic ) : Dynamic/*MISSING nsISupports*/;
	/** @throws DOMError */
	function toDataURL( ?type : String = "", ?encoderOptions : Dynamic ) : String;
	/** @throws DOMError */
	function toBlob( callback : Blob -> Void, ?type : String = "", ?encoderOptions : Dynamic ) : Void;
	
	/** Shorthand for getting a CanvasRenderingContext2D. */
	inline function getContext2d( ?attribs : {} ) : CanvasRenderingContext2D {
		return cast getContext("2d", attribs);
	}
	/** Shorthand for getting a js.html.webgl.RenderingContext. */
	inline function getContextWebGL( ?attribs : js.html.webgl.ContextAttributes ) : js.html.webgl.RenderingContext {
		return CanvasUtil.getContextWebGL(this, attribs);
	}
}

private class CanvasUtil {
	public static function getContextWebGL( canvas :CanvasElement, attribs :{} ) {
		for (name in ["webgl", "experimental-webgl"]) {
			var ctx = canvas.getContext(name, attribs);
			if (ctx != null) return ctx;
		}
		return null;
	}
}
