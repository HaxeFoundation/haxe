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

// This file is generated from mozilla\MediaStreamTrack.webidl. Do not edit!

package js.html;

typedef MediaTrackConstraintSet =
{
	@:optional var browserWindow : Int;
	@:optional var deviceId : haxe.extern.EitherType<String,haxe.extern.EitherType<Array<String>,Dynamic/*MISSING ConstrainDOMStringParameters*/>>;
	@:optional var echoCancellation : haxe.extern.EitherType<Bool,Dynamic/*MISSING ConstrainBooleanParameters*/>;
	@:optional var facingMode : haxe.extern.EitherType<String,haxe.extern.EitherType<Array<String>,Dynamic/*MISSING ConstrainDOMStringParameters*/>>;
	@:optional var frameRate : haxe.extern.EitherType<Float,Dynamic/*MISSING ConstrainDoubleRange*/>;
	@:optional var height : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
	@:optional var mediaSource : String;
	@:optional var scrollWithPage : Bool;
	@:optional var viewportHeight : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
	@:optional var viewportOffsetX : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
	@:optional var viewportOffsetY : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
	@:optional var viewportWidth : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
	@:optional var width : haxe.extern.EitherType<Int,Dynamic/*MISSING ConstrainLongRange*/>;
}