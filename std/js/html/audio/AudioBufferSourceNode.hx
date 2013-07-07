/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html.audio;

@:native("AudioBufferSourceNode")
extern class AudioBufferSourceNode extends AudioSourceNode
{
	static inline var FINISHED_STATE : Int = 3;

	static inline var PLAYING_STATE : Int = 2;

	static inline var SCHEDULED_STATE : Int = 1;

	static inline var UNSCHEDULED_STATE : Int = 0;

	/** Setter throws DOMException. */
	var buffer : AudioBuffer;

	var gain(default,null) : AudioGain;

	var loop : Bool;

	var loopEnd : Float;

	var loopStart : Float;

	var playbackRate(default,null) : AudioParam;

	var playbackState(default,null) : Int;

	@:overload( function( when : Float ) :Void {} )
	@:overload( function( when : Float, grainOffset : Float ) :Void {} )
	function start( when : Float, grainOffset : Float, grainDuration : Float ) : Void;

	function stop( when : Float ) : Void;

}
