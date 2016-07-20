/*
 * Copyright (C)2005-2016 Haxe Foundation
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

// This file is generated from mozilla\PerformanceResourceTiming.webidl line 15:0. Do not edit!

package js.html;

@:native("PerformanceResourceTiming")
extern class PerformanceResourceTiming extends PerformanceEntry
{
	var initiatorType(default,null) : String;
	var nextHopProtocol(default,null) : String;
	var redirectStart(default,null) : Float;
	var redirectEnd(default,null) : Float;
	var fetchStart(default,null) : Float;
	var domainLookupStart(default,null) : Float;
	var domainLookupEnd(default,null) : Float;
	var connectStart(default,null) : Float;
	var connectEnd(default,null) : Float;
	var secureConnectionStart(default,null) : Float;
	var requestStart(default,null) : Float;
	var responseStart(default,null) : Float;
	var responseEnd(default,null) : Float;
	var transferSize(default,null) : Int;
	var encodedBodySize(default,null) : Int;
	var decodedBodySize(default,null) : Int;
	
}