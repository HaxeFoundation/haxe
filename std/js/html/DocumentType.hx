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

/** <p><span>NOTE:&nbsp;This interface is not fully supported in Mozilla at present, including for indicating internalSubset information which Gecko generally does otherwise support.</span></p>
<p><code>DocumentType</code> inherits <a title="en/DOM/Node" rel="internal" href="https://developer.mozilla.org/en/DOM/Node">Node</a>'s properties, methods, and constants as well as the following properties of its own:</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/DocumentType">MDN</a>. */
@:native("DocumentType")
extern class DocumentType extends Node
{
    var entities (default,null) :NamedNodeMap;

    var internalSubset (default,null) :String;

    var name (default,null) :String;

    var notations (default,null) :NamedNodeMap;

    var publicId (default,null) :String;

    var systemId (default,null) :String;

    function remove () :Void;

}
