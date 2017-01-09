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

// This file is generated from mozilla\PositionError.webidl. Do not edit!

package js.html;

/**
	The `PositionError` interface represents the reason of an error occurring when using the geolocating device.

	Documentation [PositionError](https://developer.mozilla.org/en-US/docs/Web/API/PositionError) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/PositionError$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/PositionError>
**/
@:native("PositionError")
extern class PositionError
{
	static inline var PERMISSION_DENIED : Int = 1;
	static inline var POSITION_UNAVAILABLE : Int = 2;
	static inline var TIMEOUT : Int = 3;
	
	
	/**
		Returns an <code>unsigned short</code> representing the error code. The following values are possible:
		 <table class="standard-table">
		  
		   <tr>
		    Value
		    Associated constant
		    Description
		   </tr>
		   <tr>
		    <td><code>1</code></td>
		    <td><code>PERMISSION_DENIED</code></td>
		    <td>The acquisition of the geolocation information failed because the page didn't have the permission to do it.</td>
		   </tr>
		   <tr>
		    <td><code>2</code></td>
		    <td><code>POSITION_UNAVAILABLE</code></td>
		    <td>The acquisition of the geolocation failed because at least one internal source of position returned an internal error.</td>
		   </tr>
		   <tr>
		    <td><code>3</code></td>
		    <td><code>TIMEOUT</code></td>
		    <td>The time allowed to acquire the geolocation, defined by <code>PositionOptions.timeout</code> information was reached before the information was obtained.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var code(default,null) : Int;
	
	/**
		Returns a human-readable `DOMString` describing the details of the error. Specifications note that this is primarily intended for debugging use and not to be shown directly in a user interface.
	**/
	var message(default,null) : String;
	
}