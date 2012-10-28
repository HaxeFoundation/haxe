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
package cs.internal;

@:native('haxe.lang.FieldLookup')
@:keep @:static private class FieldLookup 
{

	@:private private static var fieldIds:Array<Int>;
	@:private private static var fields:Array<String>;
	
	//s cannot be null here
	private static inline function doHash(s:String):Int
	{
		var acc = 0; //alloc_int
		for (i in 0...s.length)
		{
			acc = (( 223 * (acc >> 1) + s.charCodeAt(i) ) << 1);
		}
		
		return acc >>> 1; //always positive
	}
	
	public static function lookupHash(key:Int):String
	{
		//start of binary search algorithm
		var ids = fieldIds;
		var min = 0;
		var max = ids.length;
		
		while (min < max)
		{
			var mid = min + Std.int((max - min) / 2);
			var imid = ids[mid];
			if (key < imid)
			{
				max = mid;
			} else if (key > imid) {
				min = mid + 1;
			} else {
				return fields[mid];
			}
		}
		//if not found, it's definately an error
		throw "Field not found for hash " + key;
	}
	
	public static function hash(s:String):Int
	{
		if (s == null) return 0;
		
		var key = doHash(s);
		
		//start of binary search algorithm
		var ids = fieldIds;
		var min = 0;
		var max = ids.length;
		
		while (min < max)
		{
			var mid = Std.int(min + (max - min) / 2); //overflow safe
			var imid = ids[mid];
			if (key < imid)
			{
				max = mid;
			} else if (key > imid) {
				min = mid + 1;
			} else {
				var field = fields[mid];
				if (field != s)
					return ~key; //special case
				return key;
			}
		}
		//if not found, min holds the value where we should insert the key
		ids.insert(min, key);
		fields.insert(min, s);
		return key;
	}
	
	public static function findHash(hash:Int, hashs:Array<Int>):Int
	{
		var min = 0;
		var max = hashs.length;
		
		while (min < max)
		{
			var mid = Std.int((max + min) / 2); //overflow safe
			var imid = hashs[mid];
			if (hash < imid)
			{
				max = mid;
			} else if (hash > imid) {
				min = mid + 1;
			} else {
				return mid;
			}
		}
		//if not found, return a negative value of where it should be inserted
		return ~min;
	}
	
}