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

// This file is generated from mozilla\MIDIAccess.webidl. Do not edit!

package js.html.midi;

/**
	The `MIDIAccess` interface of the Web MIDI API provides methods for listing MIDI input and output devices, and obtaining access to those devices.

	Documentation [MIDIAccess](https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess>
**/
@:native("MIDIAccess")
extern class MIDIAccess extends js.html.EventTarget {
	
	/**
		Returns an instance of `MIDIInputMap` which provides access to any available MIDI input ports.
	**/
	var inputs(default,null) : MIDIInputMap;
	
	/**
		Returns an instance of `MIDIOutputMap` which provides access to any available MIDI output ports.
	**/
	var outputs(default,null) : MIDIOutputMap;
	
	/**
		Called whenever a new MIDI port is added or an existing port changes state.
	**/
	var onstatechange : haxe.Constraints.Function;
	
	/**
		A boolean attribute indicating whether system exclusive support is enabled on the current MIDIAccess instance.
	**/
	var sysexEnabled(default,null) : Bool;
	
}