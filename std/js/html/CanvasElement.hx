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

// This file is generated from mozilla\HTMLCanvasElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLCanvasElement` interface provides properties and methods for manipulating the layout and presentation of canvas elements. The `HTMLCanvasElement` interface also inherits the properties and methods of the `HTMLElement` interface.

	Documentation [HTMLCanvasElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement>
**/
@:native("HTMLCanvasElement")
extern class CanvasElement extends Element
{
	
	/**
		Is a positive `integer` reflecting the `width` HTML attribute of the `canvas` element interpreted in CSS pixels. When the attribute is not specified, or if it is set to an invalid value, like a negative, the default value of `300` is used.
	**/
	var width : Int;
	
	/**
		Is a positive `integer` reflecting the `height` HTML attribute of the `canvas` element interpreted in CSS pixels. When the attribute is not specified, or if it is set to an invalid value, like a negative, the default value of `150` is used.
	**/
	var height : Int;
	
	/** @throws DOMError */
	
	/**
		Returns a drawing context on the canvas, or null if the context ID is not supported. A drawing context lets you draw on the canvas. Calling getContext with `"2d"` returns a `CanvasRenderingContext2D` object, whereas calling it with `"experimental-webgl"` (or `"webgl"`) returns a `WebGLRenderingContext` object. This context is only available on browsers that implement WebGL.
	**/
	function getContext( contextId : String, ?contextOptions : Dynamic ) : Dynamic/*MISSING nsISupports*/;
	/** @throws DOMError */
	
	/**
		Returns a data-URL containing a representation of the image in the format specified by the `type` parameter (defaults to `png`). The returned image is in a resolution of 96dpi.
	**/
	function toDataURL( ?type : String = "", ?encoderOptions : Dynamic ) : String;
	/** @throws DOMError */
	
	/**
		Creates a `Blob` object representing the image contained in the canvas; this file may be cached on the disk or stored in memory at the discretion of the user agent.
	**/
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
