/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package java.util;

extern interface Map<K,V>
{
	function clear():Void;
	function containsKey(obj:Dynamic):Bool;
	function containsValue(obj:Dynamic):Bool;
	function entrySet():java.util.Set<MapEntry<K,V>>;
	function get(k:Dynamic):V;
	function keySet():java.util.Set<K>;
	function put(key:K, value:V):V;
	function remove(key:Dynamic):V;
	function size():Int;
	function values():Collection<V>;
}

@:native('java.util.Map.Entry') extern interface MapEntry<K,V>
{
	function getKey():K;
	function getValue():V;
	function setValue(v:V):V;
}