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

/** DOM&nbsp;canvas elements expose the <code><a class="external" href="http://www.w3.org/TR/html5/the-canvas-element.html#htmlcanvaselement" rel="external nofollow" target="_blank" title="http://www.w3.org/TR/html5/the-canvas-element.html#htmlcanvaselement">HTMLCanvasElement</a></code> interface, which provides properties and methods for manipulating the layout and presentation of canvas elements. The <code>HTMLCanvasElement</code> interface inherits the properties and methods of the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/element">element</a></code>
 object interface.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/HTMLCanvasElement">MDN</a>. */
@:native("HTMLCanvasElement")
extern class CanvasElement extends Element
{
    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas#attr-height">height</a></code>
 HTML attribute, specifying the height of the coordinate space in CSS pixels. */
    var height :Int;

    /** Reflects the 

<code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas#attr-width">width</a></code>
 HTML attribute, specifying the width of the coordinate space in CSS pixels. */
    var width :Int;

    function getContext (contextId :String) :Dynamic;

    function toDataURL (type :String) :String;

    /** A typed shortcut for <code>getContext("2d")</code>. */
    public inline function getContext2d () :CanvasRenderingContext2D { return cast getContext("2d"); }
	
    /** A typed shortcut for <code>getContext("webgl")</code>. */
    public inline function getContext3d () : js.html.webgl.RenderingContext { return cast getContext("webgl"); }

}
