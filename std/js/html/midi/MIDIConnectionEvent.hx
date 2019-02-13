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

// This file is generated from mozilla\MIDIConnectionEvent.webidl. Do not edit!

package js.html.midi;

/**
	The `MIDIConnectionEvent` interface of the Web MIDI API is the event passed to the `onstatechange` event of the `MIDIAccess` interface and the `onstatechange` event of the `MIDIPorts` interface. This occurs any time a new port becomes available, or when a previously available port becomes unavailable. For example, this event is fired whenever a MIDI device is either plugged in to or unplugged from a computer.

	Documentation [MIDIConnectionEvent](https://developer.mozilla.org/en-US/docs/Web/API/MIDIConnectionEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MIDIConnectionEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MIDIConnectionEvent>
**/
@:native("MIDIConnectionEvent")
extern class MIDIConnectionEvent extends js.html.Event {
	
	/**
		Returns a reference to a `MIDIPort` instance for a port that has been connected or disconnected."
	**/
	var port(default,null) : MIDIPort;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : MIDIConnectionEventInit ) : Void;
}