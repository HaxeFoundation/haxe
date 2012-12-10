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
package js.html.svg;

/** <p>The <code>SVGTransformList</code> defines a list of <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/SVGTransform">SVGTransform</a></code>
 objects.</p>
<p>An <code>SVGTransformList</code> object can be designated as read only, which means that attempts to modify the object will result in an exception being thrown.</p>
<div class="geckoVersionNote"> <p>
</p><div class="geckoVersionHeading">Gecko 9.0 note<div>(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
</div></div>
<p></p> <p>Starting in Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
,the <code>SVGTransformList</code> DOM interface is now indexable and can be accessed like Arrays</p>
</div><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/SVGTransformList">MDN</a>. */
@:native("SVGTransformList")
extern class TransformList
{
    var numberOfItems (default,null) :Int;

    function appendItem (item :Transform) :Transform;

    function clear () :Void;

    function consolidate () :Transform;

    function createSVGTransformFromMatrix (matrix :Matrix) :Transform;

    function getItem (index :Int) :Transform;

    function initialize (item :Transform) :Transform;

    function insertItemBefore (item :Transform, index :Int) :Transform;

    function removeItem (index :Int) :Transform;

    function replaceItem (item :Transform, index :Int) :Transform;

}
