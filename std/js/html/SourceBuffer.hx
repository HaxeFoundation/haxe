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

// This file is generated from mozilla\SourceBuffer.webidl. Do not edit!

package js.html;

/**
	The `SourceBuffer` interface represents a chunk of media to be passed into an `HTMLMediaElement` and played, via a `MediaSource` object. This can be made up of one or several media segments.

	Documentation [SourceBuffer](https://developer.mozilla.org/en-US/docs/Web/API/SourceBuffer) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/SourceBuffer$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/SourceBuffer>
**/
@:native("SourceBuffer")
extern class SourceBuffer extends EventTarget
{
	
	/**
		Controls how the order of media segments in the `SourceBuffer` is handled, in terms of whether they can be appended in any order, or they have to be kept in a strict sequence.
	**/
	var mode : SourceBufferAppendMode;
	
	/**
		Indicates whether the `SourceBuffer` is currently being updated — i.e. whether an `SourceBuffer.appendBuffer()`, `SourceBuffer.appendStream()`, or `SourceBuffer.remove()` operation is currently in progress.
	**/
	var updating(default,null) : Bool;
	
	/**
		Returns the time ranges that are currently buffered in the `SourceBuffer`.
	**/
	var buffered(default,null) : TimeRanges;
	
	/**
		Controls the offset applied to timestamps inside media segments that are subsequently appended to the `SourceBuffer`.
	**/
	var timestampOffset : Float;
	
	/**
		Controls the timestamp for the start of the append window. This is a timestamp range that can be used to filter what media data is appended to the `SourceBuffer`. Coded media frames with timestamps wthin this range will be appended, whereas those outside the range will be filtered out.
	**/
	var appendWindowStart : Float;
	
	/**
		Controls the timestamp for the end of the append window.
	**/
	var appendWindowEnd : Float;
	
	/** @throws DOMError */
	@:overload( function( data : ArrayBuffer ) : Void {} )
	
	/**
		Appends media segment data from an `ArrayBuffer` or `ArrayBufferView` object to the `SourceBuffer`.
	**/
	function appendBuffer( data : ArrayBufferView ) : Void;
	/** @throws DOMError */
	
	/**
		Aborts the current segment and resets the segment parser.
	**/
	function abort() : Void;
	/** @throws DOMError */
	
	/**
		Removes media segments within a specific time range from the `SourceBuffer`.
	**/
	function remove( start : Float, end : Float ) : Void;
}