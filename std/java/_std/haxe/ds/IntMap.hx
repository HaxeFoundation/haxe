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

package haxe.ds;

import java.NativeArray;

/*
 * This IntMap implementation is based on khash (https://github.com/attractivechaos/klib/blob/master/khash.h)
 * Copyright goes to Attractive Chaos <attractor@live.co.uk> and his contributors
 *
 * Thanks also to Jonas Malaco Filho for his Haxe-written IntMap code inspired by Python tables.
 * (https://jonasmalaco.com/fossil/test/jonas-haxe/artifact/887b53126e237d6c68951111d594033403889304)
 */
@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int, T> {
	private static inline var HASH_UPPER = 0.7;

	private var flags:NativeArray<Int>;
	private var _keys:NativeArray<Int>;
	private var vals:NativeArray<T>;

	private var nBuckets:Int;
	private var size:Int;
	private var nOccupied:Int;
	private var upperBound:Int;

	#if !no_map_cache
	private var cachedKey:Int;
	private var cachedIndex:Int;
	#end

	public function new():Void {
		#if !no_map_cache
		cachedIndex = -1;
		#end
	}

	public function set(key:Int, value:T):Void {
		var targetIndex:Int;
		if (nOccupied >= upperBound) {
			if (nBuckets > (size << 1)) {
				resize(nBuckets - 1); // clear "deleted" elements
			} else {
				resize(nBuckets + 1);
			}
		}

		var flags = flags, _keys = _keys;
		{
			var mask = nBuckets - 1,
				hashedKey = hash(key),
				curIndex = hashedKey & mask;

			var delKey = -1, curFlag = 0;
			// to speed things up, don't loop if the first bucket is already free
			if (isEmpty(getFlag(flags, curIndex))) {
				targetIndex = curIndex;
			} else {
				var inc = getInc(hashedKey, mask), last = curIndex;
				while (!(_keys[curIndex] == key || isEmpty(curFlag = getFlag(flags, curIndex)))) {
					if (delKey == -1 && isDel(curFlag)) {
						delKey = curIndex;
					}
					curIndex = (curIndex + inc) & mask;
					#if debug
					assert(curIndex != last);
					#end
				}

				if (delKey != -1 && isEmpty(getFlag(flags, curIndex))) {
					targetIndex = delKey;
				} else {
					targetIndex = curIndex;
				}
			}
		}

		var flag = getFlag(flags, targetIndex);
		if (isEmpty(flag)) {
			_keys[targetIndex] = key;
			vals[targetIndex] = value;
			setIsBothFalse(flags, targetIndex);
			size++;
			nOccupied++;
		} else if (isDel(flag)) {
			_keys[targetIndex] = key;
			vals[targetIndex] = value;
			setIsBothFalse(flags, targetIndex);
			size++;
		} else {
			#if debug
			assert(_keys[targetIndex] == key);
			#end
			vals[targetIndex] = value;
		}
	}

	private final function lookup(key:Int):Int {
		if (nBuckets != 0) {
			var flags = flags, _keys = _keys;

			var mask = nBuckets - 1,
				k = hash(key),
				index = k & mask,
				curFlag = -1,
				inc = getInc(k, mask), /* inc == 1 for linear probing */
				last = index;
			do {
				if (_keys[index] == key) {
					if (isEmpty(curFlag = getFlag(flags, index))) {
						index = (index + inc) & mask;
						continue;
					} else if (isDel(curFlag)) {
						return -1;
					} else {
						return index;
					}
				} else {
					index = (index + inc) & mask;
				}
			} while (index != last);
		}

		return -1;
	}

	public function get(key:Int):Null<T> {
		var idx = -1;
		#if !no_map_cache
		if (cachedKey == key && ((idx = cachedIndex) != -1)) {
			return vals[idx];
		}
		#end

		idx = lookup(key);
		if (idx != -1) {
			#if !no_map_cache
			cachedKey = key;
			cachedIndex = idx;
			#end
			return vals[idx];
		}

		return null;
	}

	private function getDefault(key:Int, def:T):T {
		var idx = -1;
		#if !no_map_cache
		if (cachedKey == key && ((idx = cachedIndex) != -1)) {
			return vals[idx];
		}
		#end

		idx = lookup(key);
		if (idx != -1) {
			#if !no_map_cache
			cachedKey = key;
			cachedIndex = idx;
			#end
			return vals[idx];
		}

		return def;
	}

	public function exists(key:Int):Bool {
		var idx = -1;
		#if !no_map_cache
		if (cachedKey == key && ((idx = cachedIndex) != -1)) {
			return true;
		}
		#end

		idx = lookup(key);
		if (idx != -1) {
			#if !no_map_cache
			cachedKey = key;
			cachedIndex = idx;
			#end

			return true;
		}

		return false;
	}

	public function remove(key:Int):Bool {
		var idx = -1;
		#if !no_map_cache
		if (!(cachedKey == key && ((idx = cachedIndex) != -1)))
		#end
		{
			idx = lookup(key);
		}

		if (idx == -1) {
			return false;
		} else {
			#if !no_map_cache
			if (cachedKey == key) {
				cachedIndex = -1;
			}
			#end
			if (!isEither(getFlag(flags, idx))) {
				setIsDelTrue(flags, idx);
				--size;

				vals[idx] = null;
				// we do NOT reset the keys here, as unlike StringMap, we check for keys equality
				// and stop if we find a key that is equal to the one we're looking for
				// setting this to 0 will allow the hash to contain duplicate `0` keys
				// (see #6457)
				// _keys[idx] = 0;
			}

			return true;
		}
	}

	private final function resize(newNBuckets:Int):Void {
		// This function uses 0.25*n_bucktes bytes of working space instead of [sizeof(key_t+val_t)+.25]*n_buckets.
		var newFlags = null;
		var j = 1;
		{
			newNBuckets = roundUp(newNBuckets);
			if (newNBuckets < 4)
				newNBuckets = 4;
			if (size >= (newNBuckets * HASH_UPPER + 0.5))
				/* requested size is too small */ {
				j = 0;
			} else { /* hash table size to be changed (shrink or expand); rehash */
				var nfSize = flagsSize(newNBuckets);
				newFlags = new NativeArray(nfSize);
				for (i in 0...nfSize) {
					newFlags[i] = 0xaaaaaaaa; // isEmpty = true; isDel = false
				}
				if (nBuckets < newNBuckets) // expand
				{
					var k = new NativeArray(newNBuckets);
					if (_keys != null) {
						arrayCopy(_keys, 0, k, 0, nBuckets);
					}
					_keys = k;

					var v = new NativeArray(newNBuckets);
					if (vals != null) {
						arrayCopy(vals, 0, v, 0, nBuckets);
					}
					vals = v;
				} // otherwise shrink
			}
		}

		if (j != 0) { // rehashing is required
			#if !no_map_cache
			// resetting cache
			cachedKey = 0;
			cachedIndex = -1;
			#end

			j = -1;
			var nBuckets = nBuckets, _keys = _keys, vals = vals, flags = flags;

			var newMask = newNBuckets - 1;
			while (++j < nBuckets) {
				if (!isEither(getFlag(flags, j))) {
					var key = _keys[j];
					var val = vals[j];

					// do not set keys as 0 - see comment about #6457
					// _keys[j] = 0;
					vals[j] = cast null;
					setIsDelTrue(flags, j);
					while (true)
						/* kick-out process; sort of like in Cuckoo hashing */ {
						var k = hash(key);
						var inc = getInc(k, newMask);
						var i = k & newMask;
						while (!isEmpty(getFlag(newFlags, i))) {
							i = (i + inc) & newMask;
						}
						setIsEmptyFalse(newFlags, i);

						if (i < nBuckets && !isEither(getFlag(flags, i)))
							/* kick out the existing element */ {
							{
								var tmp = _keys[i];
								_keys[i] = key;
								key = tmp;
							} {
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

			if (nBuckets > newNBuckets)
				/* shrink the hash table */ {
				{
					var k = new NativeArray(newNBuckets);
					arrayCopy(_keys, 0, k, 0, newNBuckets);
					this._keys = k;
				} {
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

	public inline function keys():Iterator<Int> {
		return new IntMapKeyIterator(this);
	}

	public inline function iterator():Iterator<T> {
		return new IntMapValueIterator(this);
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<Int, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():IntMap<T> {
		var copied = new IntMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keys();
		for (i in it) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public function clear():Void {
		flags = null;
		_keys = null;
		vals = null;
		nBuckets = 0;
		size = 0;
		nOccupied = 0;
		upperBound = 0;
		#if !no_map_cache
		cachedKey = 0;
		cachedIndex = -1;
		#end
	}

	private static inline function assert(x:Bool):Void {
		#if debug
		if (!x)
			throw "assert failed";
		#end
	}

	private static inline function defaultK():Int
		return 0;

	private static inline function arrayCopy(sourceArray:Dynamic, sourceIndex:Int, destinationArray:Dynamic, destinationIndex:Int, length:Int):Void
		java.lang.System.arraycopy(sourceArray, sourceIndex, destinationArray, destinationIndex, length);

	private static inline function getInc(k:Int, mask:Int):Int
		return (((k) >> 3 ^ (k) << 3) | 1) & (mask);

	private static inline function hash(i:Int):Int
		return i;

	// flags represents a bit array with 2 significant bits for each index
	// one bit for deleted (1), one for empty (2)
	// so what this function does is:
	//  * gets the integer with (flags / 16)
	//  * shifts those bits to the right ((flags % 16) * 2) places
	//  * masks it with 0b11
	private static inline function getFlag(flags:NativeArray<Int>, i:Int):Int {
		return ((flags[i >> 4] >>> ((i & 0xf) << 1)) & 3);
	}

	private static inline function isDel(flag:Int):Bool {
		return (flag & 1) != 0;
	}

	private static inline function isEmpty(flag:Int):Bool {
		return (flag & 2) != 0;
	}

	private static inline function isEither(flag:Int):Bool {
		return flag != 0;
	}

	private static inline function setIsDelFalse(flags:NativeArray<Int>, i:Int):Void {
		flags[i >> 4] &= ~(1 << ((i & 0xf) << 1));
	}

	private static inline function setIsEmptyFalse(flags:NativeArray<Int>, i:Int):Void {
		flags[i >> 4] &= ~(2 << ((i & 0xf) << 1));
	}

	private static inline function setIsBothFalse(flags:NativeArray<Int>, i:Int):Void {
		flags[i >> 4] &= ~(3 << ((i & 0xf) << 1));
	}

	private static inline function setIsDelTrue(flags:NativeArray<Int>, i:Int):Void {
		flags[i >> 4] |= 1 << ((i & 0xf) << 1);
	}

	private static inline function roundUp(x:Int):Int {
		--x;
		x |= (x) >>> 1;
		x |= (x) >>> 2;
		x |= (x) >>> 4;
		x |= (x) >>> 8;
		x |= (x) >>> 16;
		return ++x;
	}

	private static inline function flagsSize(m:Int):Int
		return ((m) < 16 ? 1 : (m) >> 4);
}

@:access(haxe.ds.IntMap)
private final class IntMapKeyIterator<T> {
	var m:IntMap<T>;
	var i:Int;
	var len:Int;

	public function new(m:IntMap<T>) {
		this.i = 0;
		this.m = m;
		this.len = m.nBuckets;
	}

	public function hasNext():Bool {
		for (j in i...len) {
			if (!IntMap.isEither(IntMap.getFlag(m.flags, j))) {
				i = j;
				return true;
			}
		}
		return false;
	}

	public function next():Int {
		var ret = m._keys[i];
		#if !no_map_cache
		m.cachedIndex = i;
		m.cachedKey = ret;
		#end
		i++;
		return ret;
	}
}

@:access(haxe.ds.IntMap)
private final class IntMapValueIterator<T> {
	var m:IntMap<T>;
	var i:Int;
	var len:Int;

	public function new(m:IntMap<T>) {
		this.i = 0;
		this.m = m;
		this.len = m.nBuckets;
	}

	public function hasNext():Bool {
		for (j in i...len) {
			if (!IntMap.isEither(IntMap.getFlag(m.flags, j))) {
				i = j;
				return true;
			}
		}
		return false;
	}

	public inline function next():T {
		return m.vals[i++];
	}
}
