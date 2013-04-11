/*
 * Copyright (C)2005-2013 Haxe Foundation
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

/** <div><strong>DRAFT</strong>
<div>This page is not complete.</div>
</div>

<p></p>
<div class="note"><strong>Note:</strong> <code>DataView</code> is not yet implemented in Gecko. It is implemented in Chrome 9.</div>
<p>An <code>ArrayBuffer</code> is a useful object for representing an arbitrary chunk of data. In many cases, such data will be read from disk or from the network, and will not follow the alignment restrictions that are imposed on the <a title="en/JavaScript_typed_arrays/ArrayBufferView" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBufferView">Typed Array Views</a> described earlier. In addition, the data will often be heterogeneous in nature and have a defined byte order.</p>
<p>The <code>DataView</code> view provides a low-level interface for reading such data from and writing it to an <code><a title="en/JavaScript_typed_arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer">ArrayBuffer</a></code>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/JavaScript_typed_arrays/DataView">MDN</a>. */
@:native("DataView")
extern class DataView extends ArrayBufferView
{
	/** DataView DataView(<a title="en/JavaScript_typed_arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer">ArrayBuffer</a> buffer, optional unsigned long byteOffset, optional unsigned long byteLength);<p>Returns a new <code>DataView</code> object using the passed <a title="en/JavaScript_typed_arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer">ArrayBuffer</a> for its storage.</p>
<pre>DataView DataView(
&nbsp; ArrayBuffer buffer,
&nbsp; optional unsigned long byteOffset,
&nbsp; optional unsigned long byteLength
);
</pre>
<div id="section_6"><span id="Parameters"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>buffer</code></dt> <dd>An existing <a title="en/JavaScript_typed_arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer"><code>ArrayBuffer</code></a> to use as the storage for the new <code>DataView</code> object.</dd> <dt><code>byteOffset</code> 
<span title="">Optional</span>
</dt> <dd>The offset, in bytes, to the first byte in the specified buffer for the new view to reference. If not specified, the view of the buffer will start with the first byte.</dd> <dt><code>byteLength</code> 
<span title="">Optional</span>
</dt> <dd>The number of elements in the byte array. If unspecified, length of the view will match the buffer's length.</dd>
</dl>
</div><div id="section_7"><span id="Return_value"></span><h6 class="editable">Return value</h6>
<p>A new <code>DataView</code> object representing the specified data buffer.</p>
</div><div id="section_8"><span id="Exceptions_thrown"></span><h6 class="editable">Exceptions thrown</h6>
<dl> <dt><code>INDEX_SIZE_ERR</code></dt> <dd>The <code>byteOffset</code> and <code>byteLength</code> result in the specified view extending past the end of the buffer.</dd>
</dl>
</div> */
	function new(?arg0 : Dynamic, ?arg1 : Dynamic, ?arg2 : Dynamic) : Void;

	function getFloat32( byteOffset : Int, ?littleEndian : Bool ) : Float;

	function getFloat64( byteOffset : Int, ?littleEndian : Bool ) : Float;

	function getInt16( byteOffset : Int, ?littleEndian : Bool ) : Int;

	function getInt32( byteOffset : Int, ?littleEndian : Bool ) : Int;

	/** <p>Gets a signed 8-bit integer at the specified byte offset from the start of the view.</p>

<div id="section_11"><span id="Parameters_2"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>offset</code></dt> <dd>The offset, in byte, from the start of the view where to read the data.</dd>
</dl>
</div><div id="section_12"><span id="Exceptions_thrown_2"></span><h6 class="editable">Exceptions thrown</h6>
<dl> <dt><code>INDEX_SIZE_ERR</code></dt> <dd>The <code>byteOffset</code> is set such as it would read beyond the end of the view</dd>
</dl>
</div> Throws DOMException. */
	@:overload( function() :Dynamic {} )
	function getInt8( byteOffset : Int ) : Int;

	function getUint16( byteOffset : Int, ?littleEndian : Bool ) : Int;

	function getUint32( byteOffset : Int, ?littleEndian : Bool ) : Int;

	/** <p>Gets an unsigned 8-bit integer at the specified byte offset from the start of the view.</p>

<div id="section_14"><span id="Parameters_3"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>offset</code></dt> <dd>The offset, in byte, from the start of the view where to read the data.</dd>
</dl>
<dl> <dt><code>INDEX_SIZE_ERR</code></dt> <dd>The <code>byteOffset</code> is set such as it would read beyond the end of the view</dd>
</dl>
</div> Throws DOMException. */
	@:overload( function() :Dynamic {} )
	function getUint8( byteOffset : Int ) : Int;

	function setFloat32( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void;

	function setFloat64( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void;

	function setInt16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;

	function setInt32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;

	/** Throws DOMException. */
	@:overload( function() :Void {} )
	function setInt8( byteOffset : Int, value : Int ) : Void;

	function setUint16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;

	function setUint32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void;

	/** Throws DOMException. */
	@:overload( function() :Void {} )
	function setUint8( byteOffset : Int, value : Int ) : Void;

}
