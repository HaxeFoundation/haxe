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

// This file is generated from mozilla\AudioTrack.webidl. Do not edit!

package js.html;

/**
	The `AudioTrack` interface represents a single audio track from one of the HTML media elements, `audio` or `video`. 

	Documentation [AudioTrack](https://developer.mozilla.org/en-US/docs/Web/API/AudioTrack) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AudioTrack$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AudioTrack>
**/
@:native("AudioTrack")
extern class AudioTrack {
	
	/**
		A `DOMString` which uniquely identifies the track within the media. This ID can be used to locate a specific track within an audio track list by calling `AudioTrackList.getTrackById()`. The ID can also be used as the fragment part of the URL if the media supports seeking by media fragment per the Media Fragments URI specification.
	**/
	var id(default,null) : String;
	
	/**
		A `DOMString` specifying the category into which the track falls. For example, the main audio track would have a `kind` of `"main"`.
	**/
	var kind(default,null) : String;
	
	/**
		A `DOMString` providing a human-readable label for the track. For example, an audio commentary track for a movie might have a `label` of `"Commentary with director John Q. Public and actors John Doe and Jane Eod."` This string is empty if no label is provided.
	**/
	var label(default,null) : String;
	
	/**
		A `DOMString` specifying the audio track's primary language, or an empty string if unknown. The language is specified as a BCP 47 ({{RFC(5646)}}) language code, such as `"en-US"` or `"pt-BR"`.
	**/
	var language(default,null) : String;
	
	/**
		A Boolean value which controls whether or not the audio track's sound is enabled. Setting this value to `false` mutes the track's audio.
	**/
	var enabled : Bool;
	
}