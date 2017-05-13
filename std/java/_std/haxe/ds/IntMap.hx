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
package haxe.ds;

import java.NativeArray;

/*
 * This IntMap implementation is based on khash (https://github.com/attractivechaos/klib/blob/master/khash.h)
 * Copyright goes to Attractive Chaos <attractor@live.co.uk> and his contributors
 *
 * Thanks also to Jonas Malaco Filho for his Haxe-written IntMap code inspired by Python tables.
 * (https://jonasmalaco.com/fossil/test/jonas-haxe/artifact/887b53126e237d6c68951111d594033403889304)
 */

@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int,T>
{
	private static inline var HASH_UPPER = 0.7;

	private var flags:NativeArray<Int>;
	private var _keys:NativeArray<Int>;
	private var vals:NativeArray<T>;

	private var nBuckets:Int;
	private var size:Int;
	private var nOccupied:Int;
	private var upperBound:Int;

	private var cachedKey:Int;
	private var cachedIndex:Int;

	public function new() : Void
	{
		cachedIndex = -1;
	}

	public function set( key : Int, value : T ) : Void
	{
		var x:Int;
		if (nOccupied >= upperBound)
		{
			if (nBuckets > (size << 1))
				resize(nBuckets - 1); //clear "deleted" elements
			else
				resize(nBuckets + 1);
		}

		var flags = flags, _keys = _keys;
		{
			var mask = nBuckets - 1;
			var site = x = nBuckets;
			var k = hash(key);
			var i = k & mask;

			var delKey = -1;
			//for speed up
			if (flagIsEmpty(flags, i)) {
				x = i;
			} else {
				var inc = getInc(k, mask);
				var last = i;
				while (! (flagIsEmpty(flags, i) || _keys[i] == key) )
				{
					if (flagIsDel(flags,i) && delKey == -1)
						delKey = i;
					i = (i + inc) & mask;
#if DEBUG_HASHTBL
					if (i == last)
					{
						throw "assert";
					}
#end
				}

				if (flagIsEmpty(flags, i) && delKey != -1)
					x = delKey;
				else
					x = i;
			}
		}

		if (flagIsEmpty(flags, x))
		{
			_keys[x] = key;
			vals[x] = value;
			setIsBothFalse(flags, x);
			size++;
			nOccupied++;
		} else if (flagIsDel(flags, x)) {
			_keys[x] = key;
			vals[x] = value;
			setIsBothFalse(flags, x);
			size++;
		} else {
			assert(_keys[x] == key);
			vals[x] = value;
		}
	}

	@:final private function lookup( key : Int ) : Int
	{
		if (nBuckets != 0)
		{
			var flags = flags, _keys = _keys;

			var mask = nBuckets - 1, k = hash(key);
			var i = k & mask;
			var inc = getInc(k, mask); /* inc == 1 for linear probing */
			var last = i;
			while (!flagIsEmpty(flags, i) && (flagIsDel(flags, i) || _keys[i] != key))
			{
				i = (i + inc) & mask;
				if (i == last)
					return -1;
			}
			return isEither(flags, i) ? -1 : i;
		}

		return -1;
	}

	public function get( key : Int ) : Null<T>
	{
		var idx = -1;
		if (cachedKey == key && ( (idx = cachedIndex) != -1 ))
		{
			return vals[idx];
		}

		idx = lookup(key);
		if (idx != -1)
		{
			cachedKey = key;
			cachedIndex = idx;

			return vals[idx];
		}

		return null;
	}

	private function getDefault( key : Int, def : T ) : T
	{
		var idx = -1;
		if (cachedKey == key && ( (idx = cachedIndex) != -1 ))
		{
			return vals[idx];
		}

		idx = lookup(key);
		if (idx != -1)
		{
			cachedKey = key;
			cachedIndex = idx;

			return vals[idx];
		}

		return def;
	}

	public function exists( key : Int ) : Bool
	{
		var idx = -1;
		if (cachedKey == key && ( (idx = cachedIndex) != -1 ))
		{
			return true;
		}

		idx = lookup(key);
		if (idx != -1)
		{
			cachedKey = key;
			cachedIndex = idx;

			return true;
		}

		return false;
	}

	public function remove( key : Int ) : Bool
	{
		var idx = -1;
		if (! (cachedKey == key && ( (idx = cachedIndex) != -1 )))
		{
			idx = lookup(key);
		}

		if (idx == -1)
		{
			return false;
		} else {
			if (cachedKey == key)
				cachedIndex = -1;

			if (!isEither(flags, idx))
			{
				setIsDelTrue(flags, idx);
				--size;

				vals[idx] = null;
				_keys[idx] = 0;
			}

			return true;
		}
	}

	@:final private function resize(newNBuckets:Int) : Void
	{
		//This function uses 0.25*n_bucktes bytes of working space instead of [sizeof(key_t+val_t)+.25]*n_buckets.
		var newFlags = null;
		var j = 1;
		{
			newNBuckets = roundUp(newNBuckets);
			if (newNBuckets < 4) newNBuckets = 4;
			if (size >= (newNBuckets * HASH_UPPER + 0.5)) /* requested size is too small */
			{
				j = 0;
			} else { /* hash table size to be changed (shrink or expand); rehash */
				var nfSize = flagsSize(newNBuckets);
				newFlags = new NativeArray( nfSize );
				for (i in 0...nfSize)
					newFlags[i] = 0xaaaaaaaa;
				if (nBuckets < newNBuckets) //expand
				{
					var k = new NativeArray(newNBuckets);
					if (_keys != null)
						arrayCopy(_keys, 0, k, 0, nBuckets);
					_keys = k;

					var v = new NativeArray(newNBuckets);
					if (vals != null)
						arrayCopy(vals, 0, v, 0, nBuckets);
					vals = v;
				} //otherwise shrink
			}
		}

		if (j != 0)
		{ //rehashing is required
			//resetting cache
			cachedKey = 0;
			cachedIndex = -1;

			j = -1;
			var nBuckets = nBuckets, _keys = _keys, vals = vals, flags = flags;

			var newMask = newNBuckets - 1;
			while (++j < nBuckets)
			{
				if (!isEither(flags, j))
				{
					var key = _keys[j];
					var val = vals[j];

					setIsDelTrue(flags, j);
					while (true) /* kick-out process; sort of like in Cuckoo hashing */
					{
						var k = hash(key);
						var inc = getInc(k, newMask);
						var i = k & newMask;
						while (!flagIsEmpty(newFlags, i))
							i = (i + inc) & newMask;
						setIsEmptyFalse(newFlags, i);

						if (i < nBuckets && !isEither(flags, i)) /* kick out the existing element */
						{
							{
								var tmp = _keys[i];
								_keys[i] = key;
								key = tmp;
							}
							{
								var tmp = vals[i];
								vals[i] = val;
								val = tmp;
							}

							setIsDelTrue(flags, i); /* mark it as deleted in the old hash table */
						} else { /* write the element and jump out of the loop */
							_keys[i] = key;
							vals[i] = val;
							break;
						}
					}
				}
			}

			if (nBuckets > newNBuckets) /* shrink the hash table */
			{
				{
					var k = new NativeArray(newNBuckets);
					arrayCopy(_keys, 0, k, 0, newNBuckets);
					this._keys = k;
				}
				{
					var v = new NativeArray(newNBuckets);
					arrayCopy(vals, 0, v, 0, newNBuckets);
					this.vals = v;
				}
			}

			this.flags = newFlags;
			this.nBuckets = newNBuckets;
			this.nOccupied = size;
			this.upperBound = Std.int(newNBuckets * HASH_UPPER + .5);
		}
	}

	/**
		Returns an iterator of all keys in the hashtable.
		Implementation detail: Do not set() any new value while iterating, as it may cause a resize, which will break iteration
	**/
	public function keys() : Iterator<Int>
	{
		var i = 0;
		var len = nBuckets;
		return {
			hasNext: function() {
				for (j in i...len)
				{
					if (!isEither(flags, j))
					{
						i = j;
						return true;
					}
				}
				return false;
			},
			next: function() {
				var ret = _keys[i];
				cachedIndex = i;
				cachedKey = ret;

				i = i + 1;
				return ret;
			}
		};
	}

	/**
		Returns an iterator of all values in the hashtable.
		Implementation detail: Do not set() any new value while iterating, as it may cause a resize, which will break iteration
	**/
	public function iterator() : Iterator<T>
	{
		var i = 0;
		var len = nBuckets;
		return {
			hasNext: function() {
				for (j in i...len)
				{
					if (!isEither(flags, j))
					{
						i = j;
						return true;
					}
				}
				return false;
			},
			next: function() {
				var ret = vals[i];
				i = i + 1;
				return ret;
			}
		};
	}

	/**
		Returns an displayable representation of the hashtable content.
	**/

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	private static inline function assert(x:Bool):Void
	{
		#if debug
		if (!x) throw "assert failed";
		#end
	}

	private static inline function defaultK():Int return 0;

	private static inline function arrayCopy(sourceArray:Dynamic, sourceIndex:Int, destinationArray:Dynamic, destinationIndex:Int, length:Int):Void
		java.lang.System.arraycopy(sourceArray, sourceIndex, destinationArray, destinationIndex, length);

	private static inline function getInc(k:Int, mask:Int):Int
		return (((k) >> 3 ^ (k) << 3) | 1) & (mask);

	private static inline function hash(i:Int):Int
		return i;

	private static inline function flagIsEmpty(flag:NativeArray<Int>, i:Int):Bool
		return ( (flag[i >> 4] >>> ((i & 0xf) << 1)) & 2 ) != 0;

	private static inline function flagIsDel(flag:NativeArray<Int>, i:Int):Bool
		return ((flag[i >> 4] >>> ((i & 0xf) << 1)) & 1) != 0;

	private static inline function isEither(flag:NativeArray<Int>, i:Int):Bool
		return ((flag[i >> 4] >>> ((i & 0xf) << 1)) & 3) != 0;

	private static inline function setIsDelFalse(flag:NativeArray<Int>, i:Int):Void
		flag[i >> 4] &= ~(1 << ((i & 0xf) << 1));

	private static inline function setIsEmptyFalse(flag:NativeArray<Int>, i:Int):Void
		flag[i >> 4] &= ~(2 << ((i & 0xf) << 1));

	private static inline function setIsBothFalse(flag:NativeArray<Int>, i:Int):Void
		flag[i >> 4] &= ~(3 << ((i & 0xf) << 1));

	private static inline function setIsDelTrue(flag:NativeArray<Int>, i:Int):Void
		flag[i >> 4] |= 1 << ((i & 0xf) << 1);

	private static inline function roundUp(x:Int):Int
	{
		--x;
		x |= (x) >>> 1;
		x |= (x) >>> 2;
		x |= (x) >>> 4;
		x |= (x) >>> 8;
		x |= (x) >>> 16;
		return ++x;
	}

	private static inline function flagsSize(m:Int):Int
		return ((m) < 16? 1 : (m) >> 4);
}
