/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/HTMLMediaElement.webidl line 14:0. Do not edit!

package js.html;

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
	
	var error(default,null) : MediaError;
	var src : String;
	var currentSrc(default,null) : String;
	var crossOrigin : String;
	var networkState(default,null) : Int;
	var preload : String;
	var buffered(default,null) : TimeRanges;
	var readyState(default,null) : Int;
	var seeking(default,null) : Bool;
	var currentTime : Float;
	var duration(default,null) : Float;
	var paused(default,null) : Bool;
	var defaultPlaybackRate : Float;
	var playbackRate : Float;
	var played(default,null) : TimeRanges;
	var seekable(default,null) : TimeRanges;
	var ended(default,null) : Bool;
	var autoplay : Bool;
	var loop : Bool;
	var controls : Bool;
	var volume : Float;
	var muted : Bool;
	var defaultMuted : Bool;
	var audioTracks(default,null) : AudioTrackList;
	var videoTracks(default,null) : VideoTrackList;
	var textTracks(default,null) : TextTrackList;
	var mediaKeys(default,null) : MediaKeys;
	var onencrypted : haxe.Constraints.Function;
	var waitingFor(default,null) : MediaWaitingFor;
	
	function load() : Void;
	function canPlayType( type : String ) : String;
	/** @throws DOMError */
	function fastSeek( time : Float ) : Void;
	/** @throws DOMError */
	function play() : Void;
	/** @throws DOMError */
	function pause() : Void;
	function addTextTrack( kind : TextTrackKind, ?label : String = "", ?language : String = "" ) : TextTrack;
	function setMediaKeys( mediaKeys : MediaKeys ) : Promise<Void>;
}