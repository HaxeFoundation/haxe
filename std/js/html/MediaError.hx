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

// This file is generated from mozilla\MediaError.webidl. Do not edit!

package js.html;

/**
	The `MediaError` interface represents an error associated to a media, like a `HTMLMediaElement`.

	Documentation [MediaError](https://developer.mozilla.org/en-US/docs/Web/API/MediaError) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaError$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaError>
**/
@:native("MediaError")
extern class MediaError
{
	static inline var MEDIA_ERR_ABORTED : Int = 1;
	static inline var MEDIA_ERR_NETWORK : Int = 2;
	static inline var MEDIA_ERR_DECODE : Int = 3;
	static inline var MEDIA_ERR_SRC_NOT_SUPPORTED : Int = 4;
	
	
	/**
		An <code>unsigned short </code> that represents the error:
		 <table class="standard-table">
		  
		   <tr>
		    Name
		    Value
		    Description
		   </tr>
		  
		  
		   <tr>
		    <td><code>MEDIA_ERR_ABORTED</code></td>
		    <td><code>1</code></td>
		    <td>The fetching of the associated ressource has been aborted by the user</td>
		   </tr>
		   <tr>
		    <td><code>MEDIA_ERR_NETWORK</code></td>
		    <td><code>2</code></td>
		    <td>A network error caused the ressource to stop being fetched.</td>
		   </tr>
		   <tr>
		    <td><code>MEDIA_ERR_DECODE</code></td>
		    <td><code>3</code></td>
		    <td>A decoding error caused the ressource to stop being fetched.</td>
		   </tr>
		   <tr>
		    <td><code>MEDIA_ERR_SRC_NOT_SUPPORTED</code></td>
		    <td><code>4</code></td>
		    <td>The associated ressource has been detected to be not suitable.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var code(default,null) : Int;
	
}