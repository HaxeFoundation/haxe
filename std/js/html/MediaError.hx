/*
 * Copyright (C)2005-2019 Haxe Foundation
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
	The `MediaError` interface represents an error which occurred while handling media in an HTML media element based on `HTMLMediaElement`, such as `audio` or `video`.

	Documentation [MediaError](https://developer.mozilla.org/en-US/docs/Web/API/MediaError) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaError$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaError>
**/
@:native("MediaError")
extern class MediaError {
	static inline var MEDIA_ERR_ABORTED : Int = 1;
	static inline var MEDIA_ERR_NETWORK : Int = 2;
	static inline var MEDIA_ERR_DECODE : Int = 3;
	static inline var MEDIA_ERR_SRC_NOT_SUPPORTED : Int = 4;
	
	
	/**
		A number which represents the general type of error that occurred, as follows: `/en-US/docs/Web/API/MediaError/code`
	**/
	var code(default,null) : Int;
	
	/**
		A `DOMString` object containing a human-readable string which provides specific diagnostic information to help the reader understand the error condition which occurred; specifically, it isn't simply a summary of what the error code means, but actual diagnostic information to help in understanding what exactly went wrong. This text and its format is not defined by the specification and will vary from one `user agent` to another. If no diagnostics are available, or no explanation can be provided, this value is an empty string (`""`).
	**/
	var message(default,null) : String;
	
}