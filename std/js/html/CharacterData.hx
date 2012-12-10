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

/** <code><a title="En/DOM/Text" rel="internal" href="https://developer.mozilla.org/En/DOM/Text">Text</a></code>, <code><a title="En/DOM/Comment" rel="internal" href="https://developer.mozilla.org/En/DOM/Comment">Comment</a></code>, and <code><a title="en/DOM/CDATASection" rel="internal" href="https://developer.mozilla.org/en/DOM/CDATASection">CDATASection</a></code> all implement CharacterData, which in turn also implements <code><a class="internal" title="En/DOM/Node" rel="internal" href="https://developer.mozilla.org/en/DOM/Node">Node</a></code>. See <code>Node</code> for the remaining methods, properties, and constants.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/En/DOM/CharacterData">MDN</a>. */
@:native("CharacterData")
extern class CharacterData extends Node
{
    /** Setter throws DOMException. */
    var data :String;

    var length (default,null) :Int;

    function appendData (data :String) :Void;

    function deleteData (offset :Int, length :Int) :Void;

    function insertData (offset :Int, data :String) :Void;

    function remove () :Void;

    function replaceData (offset :Int, length :Int, data :String) :Void;

    function substringData (offset :Int, length :Int) :String;

}
