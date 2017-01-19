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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl;

/**
	The EXT_disjoint_timer_query extension is part of the WebGL API and provides a way to measure the duration of a set of GL commands, without stalling the rendering pipeline.

	Documentation [EXT_disjoint_timer_query](https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query>
**/
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
	
	
	/**
		
		 Creates a new `WebGLTimerQueryEXT`.
		 
	**/
	function createQueryEXT() : TimerQueryEXT;
	
	/**
		
		 Deletes a given `WebGLTimerQueryEXT`.
		 
	**/
	function deleteQueryEXT( query : TimerQueryEXT ) : Void;
	
	/**
		
		 Returns `true` if a given object is a `WebGLTimerQueryEXT`.
		 
	**/
	function isQueryEXT( query : TimerQueryEXT ) : Bool;
	
	/**
		The timer starts when all commands prior to `beginQueryEXT` have been fully executed.
	**/
	function beginQueryEXT( target : Int, query : TimerQueryEXT ) : Void;
	
	/**
		The timer stops when all commands prior to `endQueryEXT` have been fully executed.
	**/
	function endQueryEXT( target : Int ) : Void;
	
	/**
		
		 Records the current time into the corresponding query object.
		 
	**/
	function queryCounterEXT( query : TimerQueryEXT, target : Int ) : Void;
	
	/**
		Returns information about a query target.
	**/
	function getQueryEXT( target : Int, pname : Int ) : Dynamic;
	
	/**
		Return the state of a query object.
	**/
	function getQueryObjectEXT( query : TimerQueryEXT, pname : Int ) : Dynamic;
}