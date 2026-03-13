/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

/**
	The v8 module exposes APIs that are specific to the version of V8 built into the Node.js binary.

	Note: The APIs and implementation are subject to change at any time.
**/
@:jsRequire("v8")
extern class V8 {
	static function getHeapStatistics():V8HeapStatistics;

	/**
		Returns statistics about the V8 heap spaces, i.e. the segments which make up the V8 heap.
		Neither the ordering of heap spaces, nor the availability of a heap space can be guaranteed
		as the statistics are provided via the V8 `GetHeapSpaceStatistics` function and may change
		from one V8 version to the next.
	**/
	static function getHeapSpaceStatistics():Array<V8HeapSpaceStatistics>;

	/**
		This method can be used to programmatically set V8 command line flags. This method should be used with care.
		Changing settings after the VM has started may result in unpredictable behavior, including crashes and data loss;
		or it may simply do nothing.

		The V8 options available for a version of Node.js may be determined by running `node --v8-options`.
	**/
	static function setFlagsFromString(string:String):Void;
}

/**
	Object returned by `V8.getHeapStatistics` method.
**/
typedef V8HeapStatistics = {
	var total_heap_size:Int;
	var total_heap_size_executable:Int;
	var total_physical_size:Int;
	var total_available_size:Int;
	var used_heap_size:Int;
	var heap_size_limit:Int;
}

/**
	Object returned by `V8.getHeapSpaceStatistics` method.
**/
typedef V8HeapSpaceStatistics = {
	var space_name:String;
	var space_size:Int;
	var space_used_size:Int;
	var space_available_size:Int;
	var physical_space_size:Int;
}
