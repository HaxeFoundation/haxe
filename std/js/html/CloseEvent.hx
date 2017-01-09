/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\CloseEvent.webidl. Do not edit!

package js.html;

/**
	A `CloseEvent` is sent to clients using WebSockets when the connection is closed. This is delivered to the listener indicated by the `WebSocket` object's `onclose` attribute.

	Documentation [CloseEvent](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent>
**/
@:native("CloseEvent")
extern class CloseEvent extends Event
{
	
	/**
		Returns a `Boolean` that Indicates whether or not the connection was cleanly closed.
	**/
	var wasClean(default,null) : Bool;
	
	/**
		Returns an <code>unsigned short</code> containing the close code send by the server. The following values are permitted status codes.
		 <table id="Status_codes" class="standard-table">
		  
		   <tr>
		    <td class="header">Status code</td>
		    <td class="header">Name</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code>0</code>–<code>999</code></td>
		    <td> </td>
		    <td>Reserved and not used.</td>
		   </tr>
		   <tr>
		    <td><code>1000</code></td>
		    <td><code>CLOSE_NORMAL</code></td>
		    <td>Normal closure; the connection successfully completed whatever purpose for which it was created.</td>
		   </tr>
		   <tr>
		    <td><code>1001</code></td>
		    <td><code>CLOSE_GOING_AWAY</code></td>
		    <td>The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.</td>
		   </tr>
		   <tr>
		    <td><code>1002</code></td>
		    <td><code>CLOSE_PROTOCOL_ERROR</code></td>
		    <td>The endpoint is terminating the connection due to a protocol error.</td>
		   </tr>
		   <tr>
		    <td><code>1003</code></td>
		    <td><code>CLOSE_UNSUPPORTED</code></td>
		    <td>The connection is being terminated because the endpoint received data of a type it cannot accept (for example, a text-only endpoint received binary data).</td>
		   </tr>
		   <tr>
		    <td><code>1004</code></td>
		    <td> </td>
		    <td>Reserved. A meaning might be defined in the future.</td>
		   </tr>
		   <tr>
		    <td><code>1005</code></td>
		    <td><code>CLOSE_NO_STATUS</code></td>
		    <td>Reserved.  Indicates that no status code was provided even though one was expected.</td>
		   </tr>
		   <tr>
		    <td><code>1006</code></td>
		    <td><code>CLOSE_ABNORMAL</code></td>
		    <td>Reserved. Used to indicate that a connection was closed abnormally (that is, with no close frame being sent) when a status code is expected.</td>
		   </tr>
		   <tr>
		    <td><code>1007</code></td>
		    <td>Unsupported Data</td>
		    <td>The endpoint is terminating the connection because a message was received that contained inconsistent data (e.g., non-UTF-8 data within a text message).</td>
		   </tr>
		   <tr>
		    <td><code>1008</code></td>
		    <td>Policy Violation</td>
		    <td>The endpoint is terminating the connection because it received a message that violates its policy. This is a generic status code, used when codes 1003 and 1009 are not suitable.</td>
		   </tr>
		   <tr>
		    <td><code>1009</code></td>
		    <td><code>CLOSE_TOO_LARGE</code></td>
		    <td>The endpoint is terminating the connection because a data frame was received that is too large.</td>
		   </tr>
		   <tr>
		    <td><code>1010</code></td>
		    <td>Missing Extension</td>
		    <td>The client is terminating the connection because it expected the server to negotiate one or more extension, but the server didn't.</td>
		   </tr>
		   <tr>
		    <td><code>1011</code></td>
		    <td>Internal Error</td>
		    <td>The server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.</td>
		   </tr>
		   <tr>
		    <td><code>1012</code></td>
		    <td>Service Restart</td>
		    <td>The server is terminating the connection because it is restarting. [Ref]</td>
		   </tr>
		   <tr>
		    <td><code>1013</code></td>
		    <td>Try Again Later</td>
		    <td>The server is terminating the connection due to a temporary condition, e.g. it is overloaded and is casting off some of its clients. [Ref]</td>
		   </tr>
		   <tr>
		    <td><code>1014</code></td>
		    <td> </td>
		    <td>Reserved for future use by the WebSocket standard.</td>
		   </tr>
		   <tr>
		    <td><code>1015</code></td>
		    <td>TLS Handshake</td>
		    <td>Reserved. Indicates that the connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).</td>
		   </tr>
		   <tr>
		    <td><code>1016</code>–<code>1999</code></td>
		    <td> </td>
		    <td>Reserved for future use by the WebSocket standard.</td>
		   </tr>
		   <tr>
		    <td><code>2000</code>–<code>2999</code></td>
		    <td> </td>
		    <td>Reserved for use by WebSocket extensions.</td>
		   </tr>
		   <tr>
		    <td><code>3000</code>–<code>3999</code></td>
		    <td> </td>
		    <td>Available for use by libraries and frameworks. May not be used by applications. Available for registration at the IANA via first-come, first-serve.</td>
		   </tr>
		   <tr>
		    <td><code>4000</code>–<code>4999</code></td>
		    <td> </td>
		    <td>Available for use by applications.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var code(default,null) : Int;
	
	/**
		Returns a `DOMString` indicating the reason the server closed the connection. This is specific to the particular server and sub-protocol.
	**/
	var reason(default,null) : String;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : CloseEventInit ) : Void;
}