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

// This file is generated from mozilla\ImageData.webidl. Do not edit!

package js.html;

/**
	The `ImageData` interface represents the underlying pixel data of an area of a `canvas` element. It is created using the `ImageData()` constructor or creator methods on the `CanvasRenderingContext2D` object associated with a canvas: `createImageData()` and `getImageData()`. It can also be used to set a part of the canvas by using `putImageData()`.

	Documentation [ImageData](https://developer.mozilla.org/en-US/docs/Web/API/ImageData) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ImageData$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ImageData>
**/
@:native("ImageData")
extern class ImageData
{
	
	/**
		Is an `unsigned` `long` representing the actual width, in pixels, of the `ImageData`.
	**/
	var width(default,null) : Int;
	
	/**
		Is an `unsigned` `long` representing the actual height, in pixels, of the `ImageData`.
	**/
	var height(default,null) : Int;
	
	/**
		Is a `Uint8ClampedArray` representing a one-dimensional array containing the data in the RGBA order, with integer values between `0` and `255` (included).
	**/
	var data(default,null) : Uint8ClampedArray;
	
	/** @throws DOMError */
	@:overload( function( sw : Int, sh : Int ) : Void {} )
	function new( data : Uint8ClampedArray, sw : Int, ?sh : Int ) : Void;
}