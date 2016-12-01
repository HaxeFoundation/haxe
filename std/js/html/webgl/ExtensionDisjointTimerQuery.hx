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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl;

@:native("EXT_disjoint_timer_query")
extern class ExtensionDisjointTimerQuery
{
	static inline var QUERY_COUNTER_BITS_EXT : Int = 34916;
	static inline var CURRENT_QUERY_EXT : Int = 34917;
	static inline var QUERY_RESULT_EXT : Int = 34918;
	static inline var QUERY_RESULT_AVAILABLE_EXT : Int = 34919;
	static inline var TIME_ELAPSED_EXT : Int = 35007;
	static inline var TIMESTAMP_EXT : Int = 36392;
	static inline var GPU_DISJOINT_EXT : Int = 36795;
	
	function createQueryEXT() : TimerQueryEXT;
	function deleteQueryEXT( query : TimerQueryEXT ) : Void;
	function isQueryEXT( query : TimerQueryEXT ) : Bool;
	function beginQueryEXT( target : Int, query : TimerQueryEXT ) : Void;
	function endQueryEXT( target : Int ) : Void;
	function queryCounterEXT( query : TimerQueryEXT, target : Int ) : Void;
	function getQueryEXT( target : Int, pname : Int ) : Dynamic;
	function getQueryObjectEXT( query : TimerQueryEXT, pname : Int ) : Dynamic;
}