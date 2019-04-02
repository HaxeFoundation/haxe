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

// This file is generated from mozilla\MediaStreamTrack.webidl. Do not edit!

package js.html;

typedef MediaTrackConstraintSet = {
	var ?autoGainControl : haxe.extern.EitherType<Bool,ConstrainBooleanParameters>;
	var ?browserWindow : Int;
	var ?channelCount : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?deviceId : haxe.extern.EitherType<String,haxe.extern.EitherType<Array<String>,ConstrainDOMStringParameters>>;
	var ?echoCancellation : haxe.extern.EitherType<Bool,ConstrainBooleanParameters>;
	var ?facingMode : haxe.extern.EitherType<String,haxe.extern.EitherType<Array<String>,ConstrainDOMStringParameters>>;
	var ?frameRate : haxe.extern.EitherType<Float,ConstrainDoubleRange>;
	var ?height : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?mediaSource : String;
	var ?noiseSuppression : haxe.extern.EitherType<Bool,ConstrainBooleanParameters>;
	var ?scrollWithPage : Bool;
	var ?viewportHeight : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?viewportOffsetX : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?viewportOffsetY : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?viewportWidth : haxe.extern.EitherType<Int,ConstrainLongRange>;
	var ?width : haxe.extern.EitherType<Int,ConstrainLongRange>;
}