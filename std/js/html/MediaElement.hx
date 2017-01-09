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

// This file is generated from mozilla\HTMLMediaElement.webidl. Do not edit!

package js.html;

/**
	The `HTMLMediaElement` interface adds to `HTMLElement` the properties and methods needed to support basic media-related capabilities that are common to audio and video. The `HTMLVideoElement` and `HTMLAudioElement` elements both inherit this interface.

	Documentation [HTMLMediaElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement>
**/
@:native("HTMLMediaElement")
extern class MediaElement extends Element
{
	static inline var NETWORK_EMPTY : Int = 0;
	static inline var NETWORK_IDLE : Int = 1;
	static inline var NETWORK_LOADING : Int = 2;
	static inline var NETWORK_NO_SOURCE : Int = 3;
	static inline var HAVE_NOTHING : Int = 0;
	static inline var HAVE_METADATA : Int = 1;
	static inline var HAVE_CURRENT_DATA : Int = 2;
	static inline var HAVE_FUTURE_DATA : Int = 3;
	static inline var HAVE_ENOUGH_DATA : Int = 4;
	
	
	/**
		Returns a `MediaError` object for the most recent error, or `null` if there has not been an error.
	**/
	var error(default,null) : MediaError;
	
	/**
		Is a `DOMString` that reflects the `src` HTML attribute, which contains the URL of a media resource to use.
	**/
	var src : String;
	
	/**
		Returns a `DOMString` with the absolute URL of the chosen media resource.
	**/
	var currentSrc(default,null) : String;
	
	/**
		Is a `DOMString` indicating the CORS setting for this media element.
	**/
	var crossOrigin : String;
	
	/**
		Returns a `unsigned short` (enumeration) indicating the current state of fetching the media over the network.
	**/
	var networkState(default,null) : Int;
	
	/**
		Is a `DOMString` that reflects the `preload` HTML attribute, indicating what data should be preloaded, if any. Possible values are: `none`, `metadata`, `auto`.
	**/
	var preload : String;
	
	/**
		Returns a `TimeRanges` object that indicates the ranges of the media source that the browser has buffered (if any) at the moment the `buffered` property is accessed.
	**/
	var buffered(default,null) : TimeRanges;
	
	/**
		Returns a `unsigned short` (enumeration) indicating the readiness state of the media.
	**/
	var readyState(default,null) : Int;
	
	/**
		Returns a `Boolean` that indicates whether the media is in the process of seeking to a new position.
	**/
	var seeking(default,null) : Bool;
	
	/**
		Is a `double` indicating the current playback time in seconds. Setting this value seeks the media to the new time.
	**/
	var currentTime : Float;
	
	/**
		Returns a `double` indicating the length of the media in seconds, or 0 if no media data is available.
	**/
	var duration(default,null) : Float;
	
	/**
		Returns a `Boolean` that indicates whether the media element is paused.
	**/
	var paused(default,null) : Bool;
	
	/**
		Is a `double` indicating the default playback rate for the media.
	**/
	var defaultPlaybackRate : Float;
	
	/**
		Is a `double` that indicates the rate at which the media is being played back. 
	**/
	var playbackRate : Float;
	
	/**
		Returns a `TimeRanges` object that contains the ranges of the media source that the browser has played, if any.
	**/
	var played(default,null) : TimeRanges;
	
	/**
		Returns a `TimeRanges` object that contains the time ranges that the user is able to seek to, if any.
	**/
	var seekable(default,null) : TimeRanges;
	
	/**
		Returns a `Boolean` that indicates whether the media element has finished playing.
	**/
	var ended(default,null) : Bool;
	
	/**
		Is a `Boolean` that reflects the `autoplay` HTML attribute, indicating whether playback should automatically begin as soon as enough media is available to do so without interruption.
	**/
	var autoplay : Bool;
	
	/**
		Is a `Boolean` that reflects the `loop` HTML attribute, which indicates whether the media element should start over when it reaches the end.
	**/
	var loop : Bool;
	
	/**
		Is a `Boolean` that reflects the `controls` HTML attribute, indicating whether user interface items for controlling the resource should be displayed.
	**/
	var controls : Bool;
	
	/**
		Is a `double` indicating the audio volume, from 0.0 (silent) to 1.0 (loudest).
	**/
	var volume : Float;
	
	/**
		Is a `Boolean` that determines whether audio is muted. `true` if the audio is muted and `false` otherwise.
	**/
	var muted : Bool;
	
	/**
		Is a `Boolean` that reflects the `muted` HTML attribute, which indicates whether the media element's audio output should be muted by default.
	**/
	var defaultMuted : Bool;
	
	/**
		Is a `AudioTrackList` that lists the `AudioTrack` objects contained in the element.
	**/
	var audioTracks(default,null) : AudioTrackList;
	
	/**
		Returns the list of `VideoTrack` objects contained in the element.
		 
		 Note: Gecko supports only single track playback, and the parsing of tracks' metadata is only available for media with the Ogg container format.
		 
		 
	**/
	var videoTracks(default,null) : VideoTrackList;
	
	/**
		Returns the list of `TextTrack` objects contained in the element.
	**/
	var textTracks(default,null) : TextTrackList;
	
	/**
		Is a `MediaStream` representing the media to play or that has played in the current `HTMLMediaElement`.
	**/
	var srcObject : MediaStream;
	
	
	/**
		Resets the media element and restarts the media resource. Any pending events are discarded. How much media data is fetched is still affected by the `preload` attribute. This method can be useful for releasing resources after any `src` attribute and `source` element descendants have been removed. Otherwise, it is usually unnecessary to use this method, unless required to rescan `source` element children after dynamic changes.
	**/
	function load() : Void;
	
	/**
		Determines whether the specified media type can be played back.
	**/
	function canPlayType( type : String ) : String;
	/** @throws DOMError */
	
	/**
		Directly seeks to the given time.
	**/
	function fastSeek( time : Float ) : Void;
	/** @throws DOMError */
	
	/**
		Begins playback of the media.
	**/
	function play() : Void;
	/** @throws DOMError */
	
	/**
		Pauses the media playback.
	**/
	function pause() : Void;
	
	/**
		Adds a text track (such as a track for subtitles) to a media element.
	**/
	function addTextTrack( kind : TextTrackKind, ?label : String = "", ?language : String = "" ) : TextTrack;
}