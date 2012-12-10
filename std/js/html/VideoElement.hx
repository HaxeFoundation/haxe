/*
 * Copyright (C)2005-2012 Haxe Foundation
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

/** DOM <code>video</code> objects expose the <a class="external" title="http://www.w3.org/TR/html5/video.html#htmlvideoelement" rel="external" href="http://www.w3.org/TR/html5/video.html#htmlvideoelement" target="_blank">HTMLVideoElement</a> interface, which provides special properties (beyond the regular <a href="https://developer.mozilla.org/en/DOM/element" rel="internal">element</a> object and <a title="en/DOM/HTMLMediaElement" rel="internal" href="https://developer.mozilla.org/en/DOM/HTMLMediaElement">HTMLMediaElement</a> interfaces they also have available to them by inheritance) for manipulating video objects.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLVideoElement">MDN</a>. */
@:native("HTMLVideoElement")
extern class VideoElement extends MediaElement
{
    var decodedFrameCount (default,null) :Int;

    var displayingFullscreen (default,null) :Bool;

    var droppedFrameCount (default,null) :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/video#attr-height">height</a></code>
 HTML attribute, which specifies the height of the display area, in CSS pixels. */
    var height :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/video#attr-poster">poster</a></code>
 HTML&nbsp;attribute, which specifies an image to show while no video data is available. */
    var poster :String;

    var supportsFullscreen (default,null) :Bool;

    /** The intrinsic height of the resource in CSS pixels, taking into account the dimensions, aspect ratio, clean aperture, resolution, and so forth, as defined for the format used by the resource. If the element's ready state is HAVE_NOTHING, the value is 0. */
    var videoHeight (default,null) :Int;

    /** The intrinsic width of the resource in CSS pixels, taking into account the dimensions, aspect ratio, clean aperture, resolution, and so forth, as defined for the format used by the resource. If the element's ready state is HAVE_NOTHING, the value is 0. */
    var videoWidth (default,null) :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/video#attr-width">width</a></code>
&nbsp;HTML&nbsp;attribute, which specifies the width of the display area, in CSS pixels. */
    var width :Int;

    function enterFullScreen () :Void;

    function enterFullscreen () :Void;

    function exitFullScreen () :Void;

    function exitFullscreen () :Void;

}
