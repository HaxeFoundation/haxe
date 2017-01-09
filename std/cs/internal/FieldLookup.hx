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
package cs.internal;

@:native('haxe.lang.FieldHashConflict')
@:final @:nativeGen
@:keep class FieldHashConflict {
	@:readOnly public var hash(default,never):Int;
	@:readOnly public var name(default,never):String;
	public var value:Dynamic;
	public var next:FieldHashConflict;
	public function new(hash, name, value, next) {
		untyped this.hash = hash;
		untyped this.name = name;
		this.value = value;
		this.next = next;
	}
}

@:native('haxe.lang.FieldLookup')
@:final @:nativeGen
@:classCode("#pragma warning disable 628\n")
@:keep @:static class FieldLookup
{
	@:protected private static var fieldIds:cs.NativeArray<Int>;
	@:protected private static var fields:cs.NativeArray<String>;
	@:protected private static var length:Int;

	static function __init__()
	{
		length = fieldIds.Length;
	}

	private static function addFields(nids:cs.NativeArray<Int>, nfields:cs.NativeArray<String>):Void
	{
		// first see if we need to add anything
		var cids = fieldIds,
		    cfields = fields;
		var nlen = nids.Length;
		var clen = length;
		if (nfields.Length != nlen) throw 'Different fields length: $nlen and ${nfields.Length}';

		//TODO optimize
		var needsChange = false;
		for (i in nids)
		{
			if (findHash(i, cids, clen) < 0)
			{
				needsChange = true;
				break;
			}
		}

		// if we do, lock and merge
		if (needsChange)
		{
			cs.Lib.lock(FieldLookup, {
				// trace(cs.Lib.array(nids), cs.Lib.array(cids));
				var ansIds = new cs.NativeArray(clen + nlen),
				    ansFields = new cs.NativeArray(clen + nlen);
				var ci = 0, ni = 0, ansi = 0;
				while (ci < clen && ni < nlen)
				{
					if (cids[ci] < nids[ni])
					{
						ansIds[ansi] = cids[ci];
						ansFields[ansi] = cfields[ci];
						++ci;
					} else {
						ansIds[ansi] = nids[ni];
						ansFields[ansi] = nfields[ni];
						++ni;
					}
					++ansi;
				}

				if (ci < clen)
				{
					cs.system.Array.Copy(cids, ci, ansIds, ansi, clen - ci);
					cs.system.Array.Copy(cfields, ci, ansFields, ansi, clen - ci);
					ansi += clen - ci;
				}

				if (ni < nlen)
				{
					cs.system.Array.Copy(nids, ni, ansIds, ansi, nlen - ni);
					cs.system.Array.Copy(nfields, ni, ansFields, ansi, nlen - ni);
					ansi += nlen - ni;
				}

				// trace(cs.Lib.array(ansIds));
				fieldIds = ansIds;
				fields = ansFields;
				length = ansi;
			});
		}
	}

	//s cannot be null here
	private static inline function doHash(s:String):Int
	{
		var acc = 0; //alloc_int
		for (i in 0...s.length)
		{
			acc = (( 223 * (acc >> 1) + cast(s[i], Int)) << 1);
		}

		return acc >>> 1; //always positive
	}

	public static function lookupHash(key:Int):String
	{
		//start of binary search algorithm
		var ids = fieldIds;
		var min = 0;
		var max = length;

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
		//if not found, it's definitely an error
		throw "Field not found for hash " + key;
	}

	public static function hash(s:String):Int
	{
		if (s == null) return 0;

		var key = doHash(s);

		//start of binary search algorithm
		var ids = fieldIds,
		    fld = fields;
		var min = 0;
		var max = length;

		var len = length;

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
				var field = fld[mid];
				if (field != s)
					return ~key; //special case
				return key;
			}
		}

		//if not found, min holds the value where we should insert the key
		//ensure thread safety:
		cs.Lib.lock(FieldLookup, {
			if (len != length) //race condition which will very rarely happen - other thread modified sooner.
				return hash(s); //since we already own the lock, this second try will always succeed

#if erase_generics
			fieldIds = insertInt(fieldIds, length, min, key);
			fields = insertString(fields, length, min, s);
#else
			insert(fieldIds, length, min, key);
			insert(fields, length, min, s);
			// ids.insert(min, key);
			// fields.insert(min, s);
#end
			++length;
		});
		return key;
	}

	public static function findHash(hash:Int, hashs:cs.NativeArray<Int>, length:Int):Int
	{
		var min = 0;
		var max = length;

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

	#if !erase_generics
	static function remove<T>(a:cs.NativeArray<T>, length:Int, pos:Int)
	{
		cs.system.Array.Copy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = null;
	}

	static function insert<T>(a:cs.Ref<cs.NativeArray<T>>, length:Int, pos:Int, x:T)
	{
		var capacity = a.Length;
		if (pos == length)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				a.CopyTo(newarr, 0);
				a = newarr;
			}
		}
		else if (pos == 0)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				cs.system.Array.Copy(a, 0, newarr, 1, length);
				a = newarr;
			}
			else
			{
				cs.system.Array.Copy(a, 0, a, 1, length);
			}
		}
		else
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				cs.system.Array.Copy(a, 0, newarr, 0, pos);
				cs.system.Array.Copy(a, pos, newarr, pos + 1, length - pos);
				a = newarr;
			}
			else
			{
				cs.system.Array.Copy(a, pos, a, pos + 1, length - pos);
				cs.system.Array.Copy(a, 0, a, 0, pos);
			}
		}
		a[pos] = x;
	}
	#else
	static function removeInt(a:cs.NativeArray<Int>, length:Int, pos:Int)
	{
		cs.system.Array.Copy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = 0;
	}
	static function removeFloat(a:cs.NativeArray<Float>, length:Int, pos:Int)
	{
		cs.system.Array.Copy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = 0;
	}
	static function removeDynamic(a:cs.NativeArray<Dynamic>, length:Int, pos:Int)
	{
		cs.system.Array.Copy(a, pos + 1, a, pos, length - pos - 1);
		a[length - 1] = null;
	}

	@:extern
	static inline function __insert<T>(a:cs.NativeArray<T>, length:Int, pos:Int, x:T):cs.NativeArray<T>
	{
		var capacity = a.Length;
		if (pos == length)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				a.CopyTo(newarr, 0);
				a = newarr;
			}
		}
		else if (pos == 0)
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				cs.system.Array.Copy(a, 0, newarr, 1, length);
				a = newarr;
			}
			else
			{
				cs.system.Array.Copy(a, 0, a, 1, length);
			}
		}
		else
		{
			if (capacity == length)
			{
				var newarr = new NativeArray((length << 1) + 1);
				cs.system.Array.Copy(a, 0, newarr, 0, pos);
				cs.system.Array.Copy(a, pos, newarr, pos + 1, length - pos);
				a = newarr;
			}
			else
			{
				cs.system.Array.Copy(a, pos, a, pos + 1, length - pos);
				cs.system.Array.Copy(a, 0, a, 0, pos);
			}
		}
		a[pos] = x;
		return a;
	}

	static function insertInt(a:cs.NativeArray<Int>, length:Int, pos:Int, x:Int):cs.NativeArray<Int> return __insert(a, length, pos, x);
	static function insertFloat(a:cs.NativeArray<Float>, length:Int, pos:Int, x:Float):cs.NativeArray<Float> return __insert(a, length, pos, x);
	static function insertDynamic(a:cs.NativeArray<Dynamic>, length:Int, pos:Int, x:Dynamic):cs.NativeArray<Dynamic> return __insert(a, length, pos, x);
	static function insertString(a:cs.NativeArray<String>, length:Int, pos:Int, x:Dynamic):cs.NativeArray<String> return __insert(a, length, pos, x);
	#end

	static function getHashConflict(head:FieldHashConflict, hash:Int, name:String):FieldHashConflict {
		while (head != null) {
			if (head.hash == hash && head.name == name) {
				return head;
			}
			head = head.next;
		}
		return null;
	}

	static function setHashConflict(head:cs.Ref<FieldHashConflict>, hash:Int, name:String, value:Dynamic):Void {
		var node = head;
		while (node != null) {
			if (node.hash == hash && node.name == name) {
				node.value = value;
				return;
			}
			node = node.next;
		}
		head = new FieldHashConflict(hash, name, value, head);
	}

	static function deleteHashConflict(head:cs.Ref<FieldHashConflict>, hash:Int, name:String):Bool {
		// no conflicting fields at all
		if (head == null) {
			return false;
		}

		// list head is conflicting - just point it to the next one
		if (head.hash == hash && head.name == name) {
			head = head.next;
			return true;
		}

		// loop through the list, removing node if there's one
		var prev = head, node = head.next;
		while (node != null) {
			if (node.hash == hash && node.name == name) {
				prev.next = node.next;
				return true;
			}
			node = node.next;
		}

		// not found
		return false;
	}

	static function addHashConflictNames(head:FieldHashConflict, arr:Array<String>):Void {
		while (head != null) {
			arr.push(head.name);
			head = head.next;
		}
	}
}
