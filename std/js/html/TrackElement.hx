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

// This file is generated from mozilla\HTMLTrackElement.webidl. Do not edit!

package js.html;

/**
	The HTMLTrackElement` interface provides access to the properties of `track` elements, as well as methods to manipulate them.

	Documentation [HTMLTrackElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTrackElement) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/HTMLTrackElement$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTrackElement>
**/
@:native("HTMLTrackElement")
extern class TrackElement extends Element
{
	static inline var NONE : Int = 0;
	static inline var LOADING : Int = 1;
	static inline var LOADED : Int = 2;
	static inline var ERROR : Int = 3;
	
	
	/**
		Is a `DOMString` that reflects the `kind` HTML attribute, indicating how the text track is meant to be used. Possible values are: subtitles, captions, descriptions, chapters, metadata.
	**/
	var kind : String;
	
	/**
		Is a `DOMString` that reflects the `src` HTML attribute, indicating the address of the text track data.
	**/
	var src : String;
	
	/**
		Is a `DOMString` that reflects the `srclang` HTML attribute, indicating the language of the text track data.
	**/
	var srclang : String;
	
	/**
		Is a `DOMString` that reflects the `label` HTML attribute, indicating a user-readable title for the track.
	**/
	var label : String;
	@:native("default")
	var default_ : Bool;
	
	/**
		Returns  an <code>unsigned short</code><code> </code>that show the readiness state of the track:
		 <table style="margin: 0px 0px 1.286em;" class="standard-table">
		  
		   <tr>
		    <td class="header">Constant</td>
		    <td class="header">Value</td>
		    <td class="header">Description</td>
		   </tr>
		   <tr>
		    <td><code style="font-size: 14px;">NONE</code></td>
		    <td>0</td>
		    <td>Indicates that the text track's cues have not been obtained.</td>
		   </tr>
		   <tr>
		    <td><code style="font-size: 14px;">LOADING</code></td>
		    <td>1</td>
		    <td>Indicates that the text track is loading and there have been no fatal errors encountered so far. Further cues might still be added to the track by the parser.</td>
		   </tr>
		   <tr>
		    <td><code style="font-size: 14px;">LOADED</code></td>
		    <td>2</td>
		    <td>Indicates that the text track has been loaded with no fatal errors.</td>
		   </tr>
		   <tr>
		    <td><code style="font-size: 14px;">ERROR</code></td>
		    <td>3</td>
		    <td>Indicates that the text track was enabled, but when the user agent attempted to obtain it, this failed in some way. Some or all of the cues are likely missing and will not be obtained.</td>
		   </tr>
		  
		 </table>
		 
	**/
	var readyState(default,null) : Int;
	
	/**
		Returns `TextTrack` is the track element's text track data.
	**/
	var track(default,null) : TextTrack;
	
}