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

package php;

/**
	@see https://www.php.net/manual/en/class.arrayiterator.php
**/
@:native('ArrayIterator')
extern class ArrayIterator<K, V> implements php.ArrayAccess<K, V> implements SeekableIterator<K, V> implements Countable implements Serializable {
	@:phpClassConst static final STD_PROP_LIST:Int;
	@:phpClassConst static final ARRAY_AS_PROPS:Int;

	function new(?array:NativeArray, ?flags:Int);
	function append(value:V):Void;
	function asort():Void;
	function count():Int;
	function current():V;
	function getArrayCopy():NativeArray;
	function getFlags():Int;
	function key():K;
	function ksort():Void;
	function natcasesort():Void;
	function natsort():Void;
	function next():Void;
	function offsetExists(offset:K):Bool;
	function offsetGet(offset:K):V;
	function offsetSet(offset:K, value:V):Void;
	function offsetUnset(offset:K):Void;
	function rewind():Void;
	function seek(position:Int):Void;
	function serialize():String;
	function setFlags(flags:Int):Void;
	function uasort(cmp_function:(a:V, b:V) -> Int):Void;
	function uksort(cmp_function:(a:K, b:K) -> Int):Void;
	function unserialize(serialized:String):Void;
	function valid():Bool;
}
