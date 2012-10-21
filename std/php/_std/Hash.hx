/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

@:coreApi class Hash<T> implements php.IteratorAggregate<T> {
	private var h : ArrayAccess<T>;

	public function new() : Void {
		h = untyped __call__('array');
	}

	public function set( key : String, value : T ) : Void {
		untyped h[key] = value;
	}

	public function get( key : String ) : Null<T> {
		if (untyped __call__("array_key_exists", key, h))
			return untyped h[key];
		else
			return null;
	}

	public function exists( key : String ) : Bool {
		return untyped __call__("array_key_exists", key, h);
	}

	public function remove( key : String ) : Bool {
		if (untyped __call__("array_key_exists", key, h)) {
			untyped __call__("unset", h[key]);
			return true;
		} else
			return false;
	}

	public function keys() : Iterator<String> {
		return untyped __call__("new _hx_array_iterator", __call__("array_keys", h));
	}

	public function iterator() : Iterator<T> {
		return untyped __call__("new _hx_array_iterator", __call__("array_values", h));
	}

	public function toString() : String {
		var s = "{";
		var it = keys();
		for( i in it ) {
			s += i;
			s += " => ";
			s += Std.string(get(i));
			if( it.hasNext() )
				s += ", ";
		}
		return s + "}";
	}

	/**
		Implement IteratorAggregate for native php iteration
	**/
	#if php
	function getIterator() : Iterator<T> {
		return iterator();
	}
	#end
}
