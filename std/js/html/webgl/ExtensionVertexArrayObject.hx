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
	The OES_vertex_array_object extension is part of the WebGL API and provides vertex array objects (VAOs) which encapsulate vertex array states. These objects keep pointers to vertex data and provide names for different sets of vertex data.

	Documentation [OES_vertex_array_object](https://developer.mozilla.org/en-US/docs/Web/API/OES_vertex_array_object) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/OES_vertex_array_object$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/OES_vertex_array_object>
**/
@:native("OES_vertex_array_object")
extern class ExtensionVertexArrayObject
{
	static inline var VERTEX_ARRAY_BINDING_OES : Int = 34229;
	
	
	/**
		
		 Creates a new `WebGLVertexArrayObjectOES`.
		 
	**/
	function createVertexArrayOES() : VertexArrayObjectOES;
	
	/**
		
		 Deletes a given `WebGLVertexArrayObjectOES`.
		 
	**/
	function deleteVertexArrayOES( arrayObject : VertexArrayObjectOES ) : Void;
	
	/**
		
		 Returns `true` if a given object is a `WebGLVertexArrayObjectOES`.
		 
	**/
	function isVertexArrayOES( arrayObject : VertexArrayObjectOES ) : Bool;
	
	/**
		
		 Binds a given `WebGLVertexArrayObjectOES` to the buffer.
		 
	**/
	function bindVertexArrayOES( arrayObject : VertexArrayObjectOES ) : Void;
}