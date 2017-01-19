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
package flash;

extern class Memory {

	public static inline function select( b : flash.utils.ByteArray ) : Void {
		flash.system.ApplicationDomain.currentDomain.domainMemory = b;
	}

	public static inline function setByte( addr : Int, v : Int ) : Void {
		untyped __vmem_set__(0,addr,v);
	}

	public static inline function setI16( addr : Int, v : Int ) : Void {
		untyped __vmem_set__(1,addr,v);
	}

	public static inline function setI32( addr : Int, v : Int ) : Void {
		untyped __vmem_set__(2,addr,v);
	}

	public static inline function setFloat( addr : Int, v : Float ) : Void {
		untyped __vmem_set__(3,addr,v);
	}

	public static inline function setDouble( addr : Int, v : Float ) : Void {
		untyped __vmem_set__(4,addr,v);
	}

	public static inline function getByte( addr : Int ) : Int {
		return untyped __vmem_get__(0,addr);
	}

	public static inline function getUI16( addr : Int ) : Int {
		return untyped __vmem_get__(1,addr);
	}

	public static inline function getI32( addr : Int ) : Int {
		return untyped __vmem_get__(2,addr);
	}

	public static inline function getFloat( addr : Int ) : Float {
		return untyped __vmem_get__(3,addr);
	}

	public static inline function getDouble( addr : Int ) : Float {
		return untyped __vmem_get__(4,addr);
	}

	public static inline function signExtend1( v : Int ) : Int {
		return untyped __vmem_sign__(0,v);
	}

	public static inline function signExtend8( v : Int ) : Int {
		return untyped __vmem_sign__(1,v);
	}

	public static inline function signExtend16( v : Int ) : Int {
		return untyped __vmem_sign__(2,v);
	}

}