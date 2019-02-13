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

// This file is generated from mozilla\MediaStream.webidl. Do not edit!

package js.html;

/**
	The `MediaStreamConstraints` dictionary is used when calling `getUserMedia()` to specify what kinds of tracks should be included in the returned `MediaStream`, and, optionally, to establish constraints for those tracks' settings.

	Documentation [MediaStreamConstraints](https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamConstraints) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamConstraints$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamConstraints>
**/
typedef MediaStreamConstraints = {
	
	/**
		Either a Boolean (which indicates whether or not an audio track is requested) or a `MediaTrackConstraints` object providing the constraints which must be met by the audio track included in the returned `MediaStream`. If constraints are specified, an audio track is inherently requested.
	**/
	var ?audio : haxe.extern.EitherType<Bool,MediaTrackConstraints>;
	var ?fake : Bool;
	
	/**
		A `DOMString` identifying the peer who has sole access to the stream. If this property is specified, only the indicated peer can receive and use the stream. Streams isolated in this way can only be displayed in a media element (`audio` or `video`) where the content is protected just as if `CORS` cross-origin rules were in effect. When a peer identity is set, `MediaStreamTrack`s from that peer have their `MediaStreamTrack.isolated` flag set to `true`.
	**/
	var ?peerIdentity : String;
	var ?picture : Bool;
	
	/**
		Either a Boolean (which indicates whether or not a video track is requested) or a `MediaTrackConstraints` object providing the constraints which must be met by the video track included in the returned `MediaStream`. If constraints are specified, a video track is inherently requested.
	**/
	var ?video : haxe.extern.EitherType<Bool,MediaTrackConstraints>;
}