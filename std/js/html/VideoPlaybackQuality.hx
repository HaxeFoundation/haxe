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

// This file is generated from mozilla\VideoPlaybackQuality.webidl. Do not edit!

package js.html;

/**
	The `VideoPlaybackQuality` interface represents the set of metrics describing the playback quality of a video.

	Documentation [VideoPlaybackQuality](https://developer.mozilla.org/en-US/docs/Web/API/VideoPlaybackQuality) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/VideoPlaybackQuality$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/VideoPlaybackQuality>
**/
@:native("VideoPlaybackQuality")
extern class VideoPlaybackQuality
{
	
	/**
		A `DOMHighResTimeStamp` containing the time in miliseconds since the start of the navigation and the creation of the object.
	**/
	var creationTime(default,null) : Float;
	
	/**
		An `unsigned long` giving the number of video frames created and dropped since the creation of the associated `HTMLVideoElement`.
	**/
	var totalVideoFrames(default,null) : Int;
	
	/**
		An `unsigned long` giving the number of video frames dropped since the creation of the associated `HTMLVideoElement`.
	**/
	var droppedVideoFrames(default,null) : Int;
	
	/**
		An `unsigned long` giving the number of video frames corrupted since the creation of the associated `HTMLVideoElement`. A corrupted frame may be created or dropped.
	**/
	var corruptedVideoFrames(default,null) : Int;
	
}