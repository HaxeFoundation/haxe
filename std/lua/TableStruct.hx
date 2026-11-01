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

package lua;

import lua.Table.AnyTable;

/**
	A plain Lua table with typed field access.

	Unlike Haxe anonymous structs, this creates tables without `__fields__` metadata,
	making them suitable for passing to Lua APIs that expect plain tables.

	Example:
	```haxe
	var opts = TableStruct.create({baz: 42, qux: "hello"});
	opts.baz;  // 42 - typed field access works
	opts.qux;  // "hello"

	// Can be passed to APIs expecting AnyTable
	function process(t:AnyTable):Void {}
	process(opts);  // works via implicit conversion
	```

	Note: Haxe reflection (`Reflect.fields`, etc.) will not work on `TableStruct` values
	since they lack the `__fields__` metadata that Haxe uses for reflection.
**/
@:forward
abstract TableStruct<T:{}>(T) {
	inline function new(v:T) {
		this = v;
	}

	/**
		Creates a plain Lua table from an anonymous struct.

		The resulting table will not have `__fields__` metadata, making it
		compatible with Lua APIs that reject tables with extra fields.
	**/
	public static inline function create<T:{}>(obj:T):TableStruct<T> {
		return new TableStruct(untyped __lua_table__(obj));
	}

	/**
		Converts to `AnyTable` for use with APIs that accept any table type.
	**/
	@:to
	public inline function toAnyTable():AnyTable {
		return cast this;
	}

	/**
		Converts to `Table<String, Any>` for use with generic table APIs.
	**/
	@:to
	public inline function toTable():Table<String, Any> {
		return cast this;
	}
}
