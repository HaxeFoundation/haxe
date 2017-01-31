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

// This file is generated from mozilla\HTMLImageElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLImageElement` interface provides special properties and methods  for manipulating the layout and presentation of `img` elements.

	Documentation [HTMLImageElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement>
**/
@:native("HTMLImageElement")
extern class ImageElement extends Element
{
	
	/**
		Is a `DOMString` that reflects the `alt` HTML attribute,  thus indicating fallback context for the image.
	**/
	var alt : String;
	
	/**
		Is a `DOMString` that reflects the `src` HTML attribute, containing the full URL of the image including base URI.
	**/
	var src : String;
	
	/**
		Is a `DOMString` reflecting the `srcset` HTML attribute, containing a list of candidate images, separated by a comma (`',', U+002C COMMA`). A candidate image is a URL followed by a `'w'` with the width of the images, or an `'x'` followed by the pixel density.
	**/
	var srcset : String;
	
	/**
		Is a `DOMString` representing the CORS setting for this image element. See CORS settings attributes for further details.
	**/
	var crossOrigin : String;
	
	/**
		Is a `DOMString` that reflects the `usemap` HTML attribute, containing a partial URL of a map element.
	**/
	var useMap : String;
	
	/**
		Is a `Boolean` that reflects the `ismap` HTML attribute, indicating that the image is part of a server-side image map.
	**/
	var isMap : Bool;
	
	/**
		Is a `unsigned long` that reflects the `width` HTML attribute, indicating the rendered width of the image in CSS pixels.
	**/
	var width : Int;
	
	/**
		Is a `unsigned long` that reflects the `height` HTML attribute, indicating the rendered height of the image in CSS pixels.
	**/
	var height : Int;
	
	/**
		Returns a `unsigned long` representing the intrinsic width of the image in CSS pixels, if it is available; otherwise, it will show `0`.
	**/
	var naturalWidth(default,null) : Int;
	
	/**
		Returns a `unsigned long` representing the intrinsic height of the image in CSS pixels, if it is available; else, it shows `0`.
	**/
	var naturalHeight(default,null) : Int;
	
	/**
		Returns a `Boolean` that is `true` if the browser has finished fetching the image, whether successful or not. It also shows true, if the image has no `HTMLImageElement.src` value.
	**/
	var complete(default,null) : Bool;
	
	/**
		Is a `DOMString` representing the name of the element.
	**/
	var name : String;
	
	/**
		Is a `DOMString` indicating the alignment of the image with respect to the surrounding context.
	**/
	var align : String;
	
	/**
		Is a `long` representing the space on either side of the image.
	**/
	var hspace : Int;
	
	/**
		Is a `long` representing the space above and below the image.
	**/
	var vspace : Int;
	
	/**
		Is a `DOMString` representing the URI of a long description of the image.
	**/
	var longDesc : String;
	
	/**
		Is a `DOMString` that is responsible for the width of the border surrounding the image. This is now deprecated and the CSS `border` property should be used instead.
	**/
	var border : String;
	
	/**
		Is a `DOMString` reflecting the `sizes` HTML attribute.
	**/
	var sizes : String;
	
	/**
		Returns a `DOMString` representing the URL to the currently displayed image (which may change, for example in response to media queries).
	**/
	var currentSrc(default,null) : String;
	var lowsrc : String;
	
	/**
		Returns a `long` representing the horizontal offset from the nearest layer. This property mimics an old Netscape 4 behavior.
	**/
	var x(default,null) : Int;
	
	/**
		Returns a `long` representing the vertical offset from the nearest layer. This property is also similar to behavior of an old Netscape 4.
	**/
	var y(default,null) : Int;
	
}