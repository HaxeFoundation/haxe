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

/** <p>The <code>FileReader</code> object lets web applications asynchronously read the contents of files (or raw data buffers) stored on the user's computer, using <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/File">File</a></code>
 or <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Blob">Blob</a></code>
 objects to specify the file or data to read. File objects may be obtained in one of two ways: from a <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/FileList">FileList</a></code>
 object returned as a result of a user selecting files using the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/input">&lt;input&gt;</a></code>
 element, or from a drag and drop operation's <a title="En/DragDrop/DataTransfer" rel="internal" href="https://developer.mozilla.org/En/DragDrop/DataTransfer"><code>DataTransfer</code></a> object.</p>
<p>To create a <code>FileReader</code>, simply do the following:</p>
<pre>var reader = new FileReader();
</pre>
<p>See <a title="en/Using files from web applications" rel="internal" href="https://developer.mozilla.org/en/Using_files_from_web_applications">Using files from web applications</a> for details and examples.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/FileReader">MDN</a>. */
@:native("FileReader")
extern class FileReader extends EventTarget
{
    /** The entire read request has been completed. */
    static inline var DONE :Int = 2;

    /** No data has been loaded yet. */
    static inline var EMPTY :Int = 0;

    /** Data is currently being loaded. */
    static inline var LOADING :Int = 1;

    /** The error that occurred while reading the file. <strong>Read only.</strong> */
    var error (default,null) :FileError;

    /** Called when the read operation is aborted. */
    var onabort :EventListener;

    /** Called when an error occurs. */
    var onerror :EventListener;

    /** Called when the read operation is successfully completed. */
    var onload :EventListener;

    /** Called when the read is completed, whether successful or not. This is called after either <code>onload</code> or <code>onerror</code>. */
    var onloadend :EventListener;

    /** Called when reading the data is about to begin. */
    var onloadstart :EventListener;

    /** Called periodically while the data is being read. */
    var onprogress :EventListener;

    /** Indicates the state of the <code>FileReader</code>. This will be one of the <a rel="custom" href="https://developer.mozilla.org/en/DOM/FileReader#State_constants">State constants</a>. <strong>Read only.</strong> */
    var readyState (default,null) :Int;

    /** The file's contents. This property is only valid after the read operation is complete, and the format of the data depends on which of the methods was used to initiate the read operation. <strong>Read only.</strong> */
    var result (default,null) :Dynamic;

    function new () :Void;

    function abort () :Void;

    function readAsArrayBuffer (blob :Blob) :Void;

    function readAsBinaryString (blob :Blob) :Void;

    function readAsDataURL (blob :Blob) :Void;

    function readAsText (blob :Blob, ?encoding :String) :Void;

}
