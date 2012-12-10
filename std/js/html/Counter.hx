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

/** CSS counters are an implementation of <a class="external" rel="external" href="http://www.w3.org/TR/CSS21/generate.html#counters" title="http://www.w3.org/TR/CSS21/generate.html#counters" target="_blank">Automatic counters and numbering</a> in CSS 2.1. The value of a counter is manipulated through the use of <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/counter-reset">counter-reset</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/CSS/counter-increment">counter-increment</a></code>
 and is displayed on a page using the <code>counter()</code> or <code>counters()</code> function of the <code><a title="en/CSS/content" rel="internal" href="https://developer.mozilla.org/en/CSS/content">content</a></code> property.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/CSS_Counters">MDN</a>. */
@:native("Counter")
extern class Counter
{
    var identifier (default,null) :String;

    var listStyle (default,null) :String;

    var separator (default,null) :String;

}
