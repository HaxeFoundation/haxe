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

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl.extension;

/**
	The EXT_disjoint_timer_query extension is part of the WebGL API and provides a way to measure the duration of a set of GL commands, without stalling the rendering pipeline.

	Documentation [EXT_disjoint_timer_query](https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/EXT_disjoint_timer_query>
**/
@:native("EXT_disjoint_timer_query")
extern class EXTDisjointTimerQuery {
	
	/**
		A `GLint` indicating the number of bits used to hold the query result for the given target.
	**/
	static inline var QUERY_COUNTER_BITS_EXT : Int = 34916;
	
	/**
		A `WebGLQuery` object, which is the currently active query for the given target.
	**/
	static inline var CURRENT_QUERY_EXT : Int = 34917;
	
	/**
		A `GLuint64EXT` containing the query result.
	**/
	static inline var QUERY_RESULT_EXT : Int = 34918;
	
	/**
		A `GLboolean` indicating whether or not a query result is available.
	**/
	static inline var QUERY_RESULT_AVAILABLE_EXT : Int = 34919;
	
	/**
		Elapsed time (in nanoseconds).
	**/
	static inline var TIME_ELAPSED_EXT : Int = 35007;
	
	/**
		The current time.
	**/
	static inline var TIMESTAMP_EXT : Int = 36392;
	
	/**
		A `GLboolean` indicating whether or not the GPU performed any disjoint operation.
	**/
	static inline var GPU_DISJOINT_EXT : Int = 36795;
	
	
	/**
		
		 Creates a new `WebGLQuery`.
		 
	**/
	function createQueryEXT() : js.html.webgl.Query;
	
	/**
		
		 Deletes a given `WebGLQuery`.
		 
	**/
	function deleteQueryEXT( query : js.html.webgl.Query ) : Void;
	
	/**
		
		 Returns `true` if a given object is a `WebGLQuery`.
		 
	**/
	function isQueryEXT( query : js.html.webgl.Query ) : Bool;
	
	/**
		The timer starts when all commands prior to `beginQueryEXT` have been fully executed.
	**/
	function beginQueryEXT( target : Int, query : js.html.webgl.Query ) : Void;
	
	/**
		The timer stops when all commands prior to `endQueryEXT` have been fully executed.
	**/
	function endQueryEXT( target : Int ) : Void;
	
	/**
		
		 Records the current time into the corresponding query object.
		 
	**/
	function queryCounterEXT( query : js.html.webgl.Query, target : Int ) : Void;
	
	/**
		Returns information about a query target.
	**/
	function getQueryEXT( target : Int, pname : Int ) : Dynamic;
	
	/**
		Return the state of a query object.
	**/
	function getQueryObjectEXT( query : js.html.webgl.Query, pname : Int ) : Dynamic;
}