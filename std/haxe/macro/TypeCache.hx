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

package haxe.macro;

import haxe.macro.Type;

/**
	Data attached to types by macros: given by the build of a type
	(`setCurrent`), or computed on demand by a pure function of the type.

	A `TypeCache` is declared as the initializer of a static variable, which is
	its identity: every macro expansion starts from the same state of the
	static variables, but the content of a `TypeCache` is kept by the compiler.

	```haxe
	static var shaders = new TypeCache<ShaderData>(t -> parseShader(t));
	```
**/
class TypeCache<T> {
	final id:String;
	final compute:Null<Type->T>;

	/**
		`compute` is called for a type whose build did not give a value: its
		reads are dependencies of the value, and of the code reading it. Without
		`compute`, `get` of such a type is an error (`tryGet` returns null).
	**/
	public function new(?compute:Type->T) {
		#if macro
		id = load("type_cache_id", 0)();
		if (id == null)
			throw "A TypeCache must be the initializer of a static variable";
		#else
		id = null;
		#end
		this.compute = compute;
	}

	/**
		Gives the value of the type being built. Only during the build of that
		type, once.
	**/
	public function setCurrent(value:T):Void {
		#if macro
		load("type_cache_set", 2)(id, value);
		#end
	}

	/**
		The value of `t`: builds `t` if needed (its build can give the value),
		else uses `compute`. Records a dependency on that value.
	**/
	public function get(t:Type):T {
		#if macro
		var v:Null<{value:T}> = load("type_cache_get", 2)(id, t);
		if (v != null)
			return v.value;
		if (compute == null)
			throw "TypeCache: no value for " + Std.string(t);
		return load("type_cache_compute", 3)(id, t, compute);
		#else
		return null;
		#end
	}

	/**
		Same as `get`, without `compute`: null if the build of `t` gave no value.
	**/
	public function tryGet(t:Type):Null<T> {
		#if macro
		var v:Null<{value:T}> = load("type_cache_get", 2)(id, t);
		return v == null ? null : v.value;
		#else
		return null;
		#end
	}

	/**
		`get` of the type at `path` ("pack.Name").
	**/
	public function getByPath(path:String):T {
		#if macro
		return get(Context.getType(path));
		#else
		return null;
		#end
	}

	/**
		Every type of the compilation that has a value given by its build,
		sorted by path. Only once typing is done (`Context.onAfterTyping`,
		`Context.onGenerate`), when every build ran.
	**/
	public function all():Array<{type:Type, value:T}> {
		#if macro
		return load("type_cache_all", 1)(id);
		#else
		return [];
		#end
	}

	#if macro
	static inline function load(f:String, nargs:Int):Dynamic {
		return @:privateAccess Context.load(f, nargs);
	}
	#end
}
