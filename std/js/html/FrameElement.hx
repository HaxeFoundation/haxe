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

/** <p><code>&lt;frame&gt;</code> is an HTML element which defines a particular area in which another HTML document can be displayed. A frame should be used within a <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/frameset">&lt;frameset&gt;</a></code>
.</p>
<p>Using the <code>&lt;frame&gt;</code> element is not encouraged because of certain disadvantages such as performance problems and lack of accessibility for users with screen readers. Instead of the <code>&lt;frame&gt;</code> element, <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/iframe">&lt;iframe&gt;</a></code>
&nbsp;may be preferred.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/frame">MDN</a>. */
@:native("HTMLFrameElement")
extern class FrameElement extends Element
{
    var contentDocument (default,null) :Document;

    var contentWindow (default,null) :DOMWindow;

    var frameBorder :String;

    var height (default,null) :Int;

    var location :String;

    var longDesc :String;

    var marginHeight :String;

    var marginWidth :String;

    /** This attribute is used to labeling frames. Without labeling all links will open in the frame that they are in. */
    var name :String;

    var noResize :Bool;

    /** This attribute defines existence of scrollbar. If this attribute is not used, browser put a scrollbar when necessary. There are two choices; "yes" for showing a scrollbar even when it is not necessary and "no" for do not showing a scrollbar even when it is necessary. */
    var scrolling :String;

    /** This attribute is specify document which will be displayed by frame. */
    var src :String;

    var width (default,null) :Int;

    function getSVGDocument () :js.html.svg.Document;

}
