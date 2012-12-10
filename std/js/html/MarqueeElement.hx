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

/** Non-standard<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/HTML/Element/marquee">MDN</a>. */
@:native("HTMLMarqueeElement")
extern class MarqueeElement extends Element
{
    /** Sets how the text is scrolled within the marquee. Possible values are <code>scroll</code>, <code>slide</code> and <code>alternate</code>. If no value is specified, the default value is <code>scroll</code>. */
    var behavior :String;

    var bgColor :String;

    /** Sets the direction of the scrolling within the marquee. Possible values are <code>left</code>, <code>right</code>, <code>up</code> and <code>down</code>. If no value is specified, the default value is <code>left</code>. */
    var direction :String;

    /** Sets the height in pixels or percentage value. */
    var height :String;

    /** Sets the horizontal margin */
    var hspace :Int;

    /** Sets the number of times the marquee will scroll. If no value is specified, the default value is −1, which means the marquee will scroll continuously. Setter throws DOMException. */
    var loop :Int;

    /** Setter throws DOMException. */
    var scrollAmount :Int;

    /** Setter throws DOMException. */
    var scrollDelay :Int;

    var trueSpeed :Bool;

    /** Sets the vertical margin in pixels or percentage value. */
    var vspace :Int;

    /** Sets the width in pixels or percentage value. */
    var width :String;

    function start () :Void;

    function stop () :Void;

}
