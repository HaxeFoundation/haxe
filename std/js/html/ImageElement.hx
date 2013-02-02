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

/** DOM image objects expose the <a title="http://www.w3.org/TR/html5/embedded-content-1.html#htmlimageelement" class=" external" rel="external" href="http://www.w3.org/TR/html5/embedded-content-1.html#htmlimageelement" target="_blank">HTMLImageElement</a> (or 
<span><a rel="custom" href="https://developer.mozilla.org/en/HTML">HTML 4</a></span> <a title="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-17701901" class=" external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-HTML/html.html#ID-17701901" target="_blank"><code>HTMLImageElement</code></a>) interface, which provides special properties and methods (beyond the regular <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/element">element</a></code>
 object interface they also have available to them by inheritance) for manipulating the layout and presentation of input elements.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLImageElement">MDN</a>. */
@:native("HTMLImageElement")
extern class ImageElement extends Element
{
	/** Indicates the alignment of the image with respect to the surrounding context. */
	var align : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img#attr-alt">alt</a></code>
 HTML attribute, indicating fallback context for the image. */
	var alt : String;

	/** Width of the border around the image. */
	var border : String;

	/** True if the browser has fetched the image, and it is in a <a title="en/HTML/Element/Img#Image Format" rel="internal" href="https://developer.mozilla.org/En/HTML/Element/Img#Image_Format">supported image type</a> that was decoded without errors. */
	var complete (default,null) : Bool;

	/** The CORS setting for this image element. See <a title="en/HTML/CORS settings attributes" rel="internal" href="https://developer.mozilla.org/en/HTML/CORS_settings_attributes">CORS&nbsp;settings attributes</a> for details. */
	var crossOrigin : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img#attr-height">height</a></code>
 HTML attribute, indicating the rendered height of the image in CSS&nbsp;pixels. */
	var height : Int;

	/** Space to the left and right of the image. */
	var hspace : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img#attr-ismap">ismap</a></code>
 HTML attribute, indicating that the image is part of a server-side image map. */
	var isMap : Bool;

	/** URI of a long description of the image. */
	var longDesc : String;

	/** A reference to a low-quality (but faster to load) copy of the image. */
	var lowsrc : String;

	var name : String;

	/** Intrinsic height of the image in CSS&nbsp;pixels, if it is available; otherwise, 0. */
	var naturalHeight (default,null) : Int;

	/** Intrinsic width of the image in CSS&nbsp;pixels, if it is available; otherwise, 0. */
	var naturalWidth (default,null) : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element#attr-src">src</a></code>
 HTML attribute, containing the URL of the image. */
	var src : String;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img#attr-usemap">usemap</a></code>
 HTML attribute, containing a partial URL of a map element. */
	var useMap : String;

	/** Space above and below the image. */
	var vspace : Int;

	/** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img#attr-width">width</a></code>
 HTML attribute, indicating the rendered width of the image in CSS&nbsp;pixels. */
	var width : Int;

	var x (default,null) : Int;

	var y (default,null) : Int;

}
