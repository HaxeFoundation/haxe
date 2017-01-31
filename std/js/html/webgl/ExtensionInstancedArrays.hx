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
	The `ANGLE_instanced_arrays` extension is part of the WebGL API and allows to draw the same object, or groups of similar objects multiple times, if they share the same vertex data, primitive count and type.

	Documentation [ANGLE_instanced_arrays](https://developer.mozilla.org/en-US/docs/Web/API/ANGLE_instanced_arrays) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ANGLE_instanced_arrays$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ANGLE_instanced_arrays>
**/
@:native("ANGLE_instanced_arrays")
extern class ExtensionInstancedArrays
{
	static inline var VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE : Int = 35070;
	
	
	/**
		
		 Behaves identically to `WebGLRenderingContext.drawArrays()` except that multiple instances of the range of elements are executed, and the instance advances for each iteration.
		 
	**/
	function drawArraysInstancedANGLE( mode : Int, first : Int, count : Int, primcount : Int ) : Void;
	
	/**
		
		 Behaves identically to `WebGLRenderingContext.drawElements()` except that multiple instances of the set of elements are executed and the instance advances between each set.
		 
	**/
	function drawElementsInstancedANGLE( mode : Int, count : Int, type : Int, offset : Int, primcount : Int ) : Void;
	
	/**
		
		 Modifies the rate at which generic vertex attributes advance when rendering multiple instances of primitives with `ANGLE_instanced_arrays.drawArraysInstancedANGLE()` and `ANGLE_instanced_arrays.drawElementsInstancedANGLE()`.
		 
	**/
	function vertexAttribDivisorANGLE( index : Int, divisor : Int ) : Void;
}