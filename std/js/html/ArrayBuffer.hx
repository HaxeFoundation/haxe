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

/** The <code>ArrayBuffer</code> is a data type that is used to represent a generic, fixed-length binary data buffer. You can't directly manipulate the contents of an <code>ArrayBuffer</code>; instead, you create an <a title="en/JavaScript typed arrays/ArrayBufferView" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBufferView"><code>ArrayBufferView</code></a> object which represents the buffer in a specific format, and use that to read and write the contents of the buffer.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer">MDN</a>. */
@:native("ArrayBuffer")
extern class ArrayBuffer
{
    /** The size, in bytes, of the array. This is established when the array is constructed and cannot be changed. <strong>Read only.</strong> */
    var byteLength (default,null) :Int;

    function new (?arg0 :Dynamic) :Void;

    function slice (begin :Int, ?end :Int) :ArrayBuffer;

}
