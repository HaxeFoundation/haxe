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

/** <p>The <code>FileReaderSync</code> interface allows to read <code>File</code> or <code>Blob</code> objects in a synchronous way.</p>
<p>This interface is <a title="https://developer.mozilla.org/En/DOM/Worker/Functions_available_to_workers" rel="internal" href="https://developer.mozilla.org/En/DOM/Worker/Functions_available_to_workers">only available</a> in <a title="Worker" rel="internal" href="https://developer.mozilla.org/En/DOM/Worker">workers</a> as it enables synchronous I/O that could potentially block.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/FileReaderSync">MDN</a>. */
@:native("FileReaderSync")
extern class FileReaderSync
{
	function new() : Void;

	function readAsArrayBuffer( blob : Blob ) : ArrayBuffer;

	function readAsBinaryString( blob : Blob ) : String;

	function readAsDataURL( blob : Blob ) : String;

	function readAsText( blob : Blob, ?encoding : String ) : String;

}
