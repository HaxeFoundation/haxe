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

// This file is generated from mozilla\MediaStreamTrackEvent.webidl. Do not edit!

package js.html;

/**
	The `MediaStreamTrackEvent` interface represents events which indicate that a `MediaStream` has had tracks added to or removed from the stream through calls to Media Stream API methods. These events are sent to the stream when these changes occur.

	Documentation [MediaStreamTrackEvent](https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrackEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrackEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrackEvent>
**/
@:native("MediaStreamTrackEvent")
extern class MediaStreamTrackEvent extends Event {
	
	/**
		A `MediaStreamTrack` object representing the track which was added to the stream.
	**/
	var track(default,null) : MediaStreamTrack;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : MediaStreamTrackEventInit ) : Void;
}