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

// This file is generated from mozilla\HTMLVideoElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLVideoElement` interface provides special properties and methods for manipulating video objects. It also inherits properties and methods of `HTMLMediaElement` and `HTMLElement`.

	Documentation [HTMLVideoElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement>
**/
@:native("HTMLVideoElement")
extern class VideoElement extends MediaElement
{
	
	/**
		Is a `DOMString` that reflects the `width` HTML attribute, which specifies the width of the display area, in CSS pixels.
	**/
	var width : Int;
	
	/**
		Is a `DOMString` that reflects the `height` HTML attribute, which specifies the height of the display area, in CSS pixels.
	**/
	var height : Int;
	
	/**
		Returns an `unsigned long` containing the intrinsic width of the resource in CSS pixels, taking into account the dimensions, aspect ratio, clean aperture, resolution, and so forth, as defined for the format used by the resource. If the element's ready state is `HAVE_NOTHING`, the value is `0`.
	**/
	var videoWidth(default,null) : Int;
	
	/**
		Returns an `unsigned long` containing the intrinsic height of the resource in CSS pixels, taking into account the dimensions, aspect ratio, clean aperture, resolution, and so forth, as defined for the format used by the resource. If the element's ready state is `HAVE_NOTHING`, the value is `0`.
	**/
	var videoHeight(default,null) : Int;
	
	/**
		Is a `DOMString` that reflects the `poster` HTML attribute, which specifies an image to show while no video data is available.
	**/
	var poster : String;
	
	
	/**
		Returns a `VideoPlaybackQuality` objects that contains the current playback metrics.
	**/
	function getVideoPlaybackQuality() : VideoPlaybackQuality;
}