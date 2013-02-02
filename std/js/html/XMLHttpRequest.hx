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

/** <p><code>XMLHttpRequest</code> is a <a class="internal" title="En/JavaScript" rel="internal" href="https://developer.mozilla.org/en/JavaScript">JavaScript</a> object that was designed by Microsoft and adopted by Mozilla, Apple, and Google. It's now being <a class="external" title="http://www.w3.org/TR/XMLHttpRequest/" rel="external" href="http://www.w3.org/TR/XMLHttpRequest/" target="_blank">standardized in the W3C</a>. It provides an easy way to retrieve data at a URL. Despite its name, <code>XMLHttpRequest</code> can be used to retrieve any type of data, not just XML, and it supports protocols other than <a title="en/HTTP" rel="internal" href="https://developer.mozilla.org/en/HTTP">HTTP</a> (including <code>file</code> and <code>ftp</code>).</p>
<p>To create an instance of <code>XMLHttpRequest</code>, simply do this:</p>
<pre>var req = new XMLHttpRequest();
</pre>
<p>For details about how to use <code>XMLHttpRequest</code>, see <a class="internal" title="En/Using XMLHttpRequest" rel="internal" href="https://developer.mozilla.org/en/DOM/XMLHttpRequest/Using_XMLHttpRequest">Using XMLHttpRequest</a>.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/XMLHttpRequest">MDN</a>. */
@:native("XMLHttpRequest")
extern class XMLHttpRequest extends EventTarget
{
	/** The operation is complete. */
	static inline var DONE : Int = 4;

	/** <code>send()</code> has been called, and headers and status are available. */
	static inline var HEADERS_RECEIVED : Int = 2;

	/** Downloading; <code>responseText</code> holds partial data. */
	static inline var LOADING : Int = 3;

	/** <code>send()</code>has not been called yet. */
	static inline var OPENED : Int = 1;

	/** <code>open()</code>has not been called yet. */
	static inline var UNSENT : Int = 0;

	var onabort : EventListener;

	var onerror : EventListener;

	var onload : EventListener;

	var onloadend : EventListener;

	var onloadstart : EventListener;

	var onprogress : EventListener;

	var onreadystatechange : EventListener;

	/** <p>The state of the request:</p> <table class="standard-table"> <tbody> <tr> <td class="header">Value</td> <td class="header">State</td> <td class="header">Description</td> </tr> <tr> <td><code>0</code></td> <td><code>UNSENT</code></td> <td><code>open()</code>has not been called yet.</td> </tr> <tr> <td><code>1</code></td> <td><code>OPENED</code></td> <td><code>send()</code>has not been called yet.</td> </tr> <tr> <td><code>2</code></td> <td><code>HEADERS_RECEIVED</code></td> <td><code>send()</code> has been called, and headers and status are available.</td> </tr> <tr> <td><code>3</code></td> <td><code>LOADING</code></td> <td>Downloading; <code>responseText</code> holds partial data.</td> </tr> <tr> <td><code>4</code></td> <td><code>DONE</code></td> <td>The operation is complete.</td> </tr> </tbody> </table> */
	var readyState (default,null) : Int;

	/** The response entity body according to <code><a href="#responseType">responseType</a></code>, as an <a title="en/JavaScript typed arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer"><code>ArrayBuffer</code></a>, <a title="en/DOM/Blob" rel="internal" href="https://developer.mozilla.org/en/DOM/Blob"><code>Blob</code></a>, <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Document">Document</a></code>
, JavaScript object (for "moz-json"), or string. This is <code>NULL</code>&nbsp;if the request is not complete or was not successful. Getter throws DOMException. */
	var response (default,null) : Dynamic;

	/** The response to the request as text, or <code>null</code> if the request was unsuccessful or has not yet been sent. <strong>Read-only.</strong> Getter throws DOMException. */
	var responseText (default,null) : String;

	/** <p>Can be set to change the response type. This tells the server what format you want the response to be in.</p> <table class="standard-table"> <tbody> <tr> <td class="header">Value</td> <td class="header">Data type of <code>response</code> property</td> </tr> <tr> <td><em>empty string</em></td> <td>String (this is the default)</td> </tr> <tr> <td>"arraybuffer"</td> <td><a title="en/JavaScript typed arrays/ArrayBuffer" rel="internal" href="https://developer.mozilla.org/en/JavaScript_typed_arrays/ArrayBuffer"><code>ArrayBuffer</code></a></td> </tr> <tr> <td>"blob"</td> <td><code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Blob">Blob</a></code>
</td> </tr> <tr> <td>"document"</td> <td><code><a rel="custom" href="https://developer.mozilla.org/en/DOM/Document">Document</a></code>
</td> </tr> <tr> <td>"text"</td> <td>String</td> </tr> <tr> <td>"moz-json"</td> <td>JavaScript object, parsed from a JSON string returned by the server 
<span title="(Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
">Requires Gecko 9.0</span>
</td> </tr> </tbody> </table> Setter throws DOMException. */
	var responseType : String;

	/** <p>The response to the request as a DOM <code><a class="internal" title="En/DOM/Document" rel="internal" href="https://developer.mozilla.org/en/DOM/document">Document</a></code> object, or <code>null</code> if the request was unsuccessful, has not yet been sent, or cannot be parsed as XML. The response is parsed as if it were a <code>text/xml</code> stream. <strong>Read-only.</strong></p> <div class="note"><strong>Note:</strong> If the server doesn't apply the <code>text/xml</code> Content-Type header, you can use <code>overrideMimeType()</code>to force <code>XMLHttpRequest</code> to parse it as XML anyway.</div> Getter throws DOMException. */
	var responseXML (default,null) : Document;

	/** The status of the response to the request. This is the HTTP result code (for example, <code>status</code> is 200 for a successful request). <strong>Read-only.</strong> Getter throws DOMException. */
	var status (default,null) : Int;

	/** The response string returned by the HTTP server. Unlike <code>status</code>, this includes the entire text of the response message ("<code>200 OK</code>", for example). <strong>Read-only.</strong> Getter throws DOMException. */
	var statusText (default,null) : String;

	/** The upload process can be tracked by adding an event listener to <code>upload</code>. 
<span>New in <a rel="custom" href="https://developer.mozilla.org/en/Firefox_3.5_for_developers">Firefox 3.5</a></span> */
	var upload (default,null) : XMLHttpRequestUpload;

	/** <p>Indicates whether or not cross-site Access-Control requests should be made using credentials such as cookies or authorization headers. 
<span>New in <a rel="custom" href="https://developer.mozilla.org/en/Firefox_3.5_for_developers">Firefox 3.5</a></span>
</p> <div class="note"><strong>Note:</strong> This never affects same-site requests.</div> <p>The default is <code>false</code>.</p> Setter throws DOMException. */
	var withCredentials : Bool;

	function new() : Void;

	function abort() : Void;

	function getAllResponseHeaders() : String;

	function getResponseHeader( header : String ) : String;

	function open( method : String, url : String, ?async : Bool, ?user : String, ?password : String ) : Void;

	function overrideMimeType( override_ : String ) : Void;

	/** <p>Sends the request. If the request is asynchronous (which is the default), this method returns as soon as the request is sent. If the request is synchronous, this method doesn't return until the response has arrived.</p>
<div class="note"><strong>Note:</strong> Any event listeners you wish to set must be set before calling <code>send()</code>.</div>

<div id="section_11"><span id="Parameters_3"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>body</code></dt> <dd>This may be an <code>nsIDocument</code>, <code>nsIInputStream</code>, or a string (an <code>nsISupportsString</code> if called from native code) that is used to populate the body of a POST request. Starting with Gecko 1.9.2, you may also specify an DOM<code><a rel="custom" href="https://developer.mozilla.org/en/DOM/File">File</a></code>
 , and starting with Gecko 2.0 (Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
 you may also specify a <a title="en/XMLHttpRequest/FormData" rel="internal" href="https://developer.mozilla.org/en/DOM/XMLHttpRequest/FormData"><code>FormData</code></a> object.</dd>
</dl>
</div> Throws DOMException. */
	@:overload( function() :Void {} )
	@:overload( function( data : ArrayBuffer ) :Void {} )
	@:overload( function( data : ArrayBufferView ) :Void {} )
	@:overload( function( data : Blob ) :Void {} )
	@:overload( function( data : Document ) :Void {} )
	@:overload( function( data : String ) :Void {} )
	function send( data : DOMFormData ) : Void;

	function setRequestHeader( header : String, value : String ) : Void;

}
