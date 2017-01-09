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

// This file is generated from mozilla\OfflineAudioCompletionEvent.webidl. Do not edit!

package js.html.audio;

/**
	The Web Audio API `OfflineAudioCompletionEvent` interface represents events that occur when the processing of an `OfflineAudioContext` is terminated. The `complete` event implements this interface.

	Documentation [OfflineAudioCompletionEvent](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioCompletionEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioCompletionEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioCompletionEvent>
**/
@:native("OfflineAudioCompletionEvent")
extern class OfflineAudioCompletionEvent extends js.html.Event
{
	var renderedBuffer(default,null) : AudioBuffer;
	
}