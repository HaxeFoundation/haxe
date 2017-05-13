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

@:coreApi class StringMap<T> implements haxe.Constraints.IMap<String,T>
{
	@:extern private static inline var HASH_UPPER = 0.77;
	@:extern private static inline var FLAG_EMPTY = 0;
	@:extern private static inline var FLAG_DEL = 1;

	/**
	 * This is the most important structure here and the reason why it's so fast.
	 * It's an array of all the hashes contained in the table. These hashes cannot be 0 nor 1,
	 * which stand for "empty" and "deleted" states.
	 *
	 * The lookup algorithm will keep looking until a 0 or the key wanted is found;
	 * The insertion algorithm will do the same but will also break when FLAG_DEL is found;
	 */
	private var hashes:NativeArray<HashType>;
	private var _keys:NativeArray<String>;
	private var vals:NativeArray<T>;

	private var nBuckets:Int;
	private var size:Int;
	private var nOccupied:Int;
	private var upperBound:Int;

	private var cachedKey:String;
	private var cachedIndex:Int;

#if DEBUG_HASHTBL
	private var totalProbes:Int;
	private var probeTimes:Int;
	private var sameHash:Int;
	private var maxProbe:Int;
#end

	public function new() : Void
	{
		cachedIndex = -1;
	}

	public function set( key : String, value : T ) : Void
	{
		var x:Int, k:Int;
		if (nOccupied >= upperBound)
		{
			if (nBuckets > (size << 1))
				resize(nBuckets - 1); //clear "deleted" elements
			else
				resize(nBuckets + 2);
		}

		var hashes = hashes, keys = _keys, hashes = hashes;
		{
			var mask = (nBuckets == 0) ? 0 : nBuckets - 1;
			var site = x = nBuckets;
			k = hash(key);
			var i = k & mask, nProbes = 0;

			var delKey = -1;
			//for speed up
			if (isEmpty(hashes[i])) {
				x = i;
			} else {
				//var inc = getInc(k, mask);
				var last = i, flag;
				while(! (isEmpty(flag = hashes[i]) || (flag == k && _keys[i] == key)) )
				{
					if (isDel(flag) && delKey == -1)
						delKey = i;
					i = (i + ++nProbes) & mask;
#if DEBUG_HASHTBL
					probeTimes++;
					if (i == last)
						throw "assert";
#end
				}

				if (isEmpty(flag) && delKey != -1)
					x = delKey;
				else
					x = i;
			}

#if DEBUG_HASHTBL
			if (nProbes > maxProbe)
				maxProbe = nProbes;
			totalProbes++;
#end
		}

		var flag = hashes[x];
		if (isEmpty(flag))
		{
			keys[x] = key;
			vals[x] = value;
			hashes[x] = k;
			size++;
			nOccupied++;
		} else if (isDel(flag)) {
			keys[x] = key;
			vals[x] = value;
			hashes[x] = k;
			size++;
		} else {
			assert(_keys[x] == key);
			vals[x] = value;
		}

		cachedIndex = x;
		cachedKey = key;
	}

	@:final private function lookup( key : String ) : Int
	{
		if (nBuckets != 0)
		{
			var hashes = hashes, keys = _keys;

			var mask = nBuckets - 1, hash = hash(key), k = hash, nProbes = 0;
			var i = k & mask;
			var last = i, flag;
			//var inc = getInc(k, mask);
			while (!isEmpty(flag = hashes[i]) && (isDel(flag) || flag != k || keys[i] != key))
			{
				i = (i + ++nProbes) & mask;
#if DEBUG_HASHTBL
				probeTimes++;
				if (i == last)
					throw "assert";
#end
			}

#if DEBUG_HASHTBL
			if (nProbes > maxProbe)
				maxProbe = nProbes;
			totalProbes++;
#end
			return isEither(flag) ? -1 : i;
		}

		return -1;
	}

	@:final @:private function resize(newNBuckets:Int) : Void
	{
		//This function uses 0.25*n_bucktes bytes of working space instead of [sizeof(key_t+val_t)+.25]*n_buckets.
		var newHash = null;
		var j = 1;
		{
			newNBuckets = roundUp(newNBuckets);
			if (newNBuckets < 4) newNBuckets = 4;
			if (size >= (newNBuckets * HASH_UPPER + 0.5)) /* requested size is too small */
			{
				j = 0;
			} else { /* hash table size to be changed (shrink or expand); rehash */
				var nfSize = newNBuckets;
				newHash = new NativeArray( nfSize );
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
			cachedKey = null;
			cachedIndex = -1;

			j = -1;
			var nBuckets = nBuckets, _keys = _keys, vals = vals, hashes = hashes;

			var newMask = newNBuckets - 1;
			while (++j < nBuckets)
			{
				var k;
				if (!isEither(k = hashes[j]))
				{
					var key = _keys[j];
					var val = vals[j];

					hashes[j] = FLAG_DEL;
					while (true) /* kick-out process; sort of like in Cuckoo hashing */
					{
						var nProbes = 0;
						//var inc = getInc(k, newMask);
						var i = k & newMask;

						while (!isEmpty(newHash[i]))
							i = (i + ++nProbes) & newMask;

						newHash[i] = k;

						if (i < nBuckets && !isEither(k = hashes[i])) /* kick out the existing element */
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

							hashes[i] = FLAG_DEL; /* mark it as deleted in the old hash table */
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

			this.hashes = newHash;
			this.nBuckets = newNBuckets;
			this.nOccupied = size;
			this.upperBound = Std.int(newNBuckets * HASH_UPPER + .5);
		}
	}

	public function get( key : String ) : Null<T>
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

	private function getDefault( key : String, def : T ) : T
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

	public function exists( key : String ) : Bool
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

	public function remove( key : String ) : Bool
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

			hashes[idx] = FLAG_DEL;
			_keys[idx] = null;
			vals[idx] = null;
			--size;

			return true;
		}
	}

	/**
		Returns an iterator of all keys in the hashtable.
		Implementation detail: Do not set() any new value while iterating, as it may cause a resize, which will break iteration
	**/
	public function keys() : Iterator<String>
	{
		var i = 0;
		var len = nBuckets;
		return {
			hasNext: function() {
				for (j in i...len)
				{
					if (!isEither(hashes[j]))
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
					if (!isEither(hashes[j]))
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

	@:extern private static inline function roundUp(x:Int):Int
	{
		--x;
		x |= (x) >>> 1;
		x |= (x) >>> 2;
		x |= (x) >>> 4;
		x |= (x) >>> 8;
		x |= (x) >>> 16;
		return ++x;
	}

	@:extern private static inline function getInc(k:Int, mask:Int):Int //return 1 for linear probing
		return (((k) >> 3 ^ (k) << 3) | 1) & (mask);

	@:extern private static inline function isEither(v:HashType):Bool
		return (v & 0xFFFFFFFE) == 0;

	@:extern private static inline function isEmpty(v:HashType):Bool
		return v == FLAG_EMPTY;

	@:extern private static inline function isDel(v:HashType):Bool
		return v == FLAG_DEL;

	//guarantee: Whatever this function is, it will never return 0 nor 1
	@:extern private static inline function hash(s:String):HashType
	{
		var k:Int = untyped s.hashCode();
		//k *= 357913941;
		//k ^= k << 24;
		//k += ~357913941;
		//k ^= k >> 31;
		//k ^= k << 31;

		k = (k+0x7ed55d16) + (k<<12);
		k = (k^0xc761c23c) ^ (k>>19);
		k = (k+0x165667b1) + (k<<5);
		k = (k+0xd3a2646c) ^ (k<<9);
		k = (k+0xfd7046c5) + (k<<3);
		k = (k^0xb55a4f09) ^ (k>>16);

		var ret = k;
		if (isEither(ret))
		{
			if (ret == 0)
				ret = 2;
			else
				ret = 0xFFFFFFFF;
		}

		return ret;
	}

	@:extern private static inline function arrayCopy(sourceArray:Dynamic, sourceIndex:Int, destinationArray:Dynamic, destinationIndex:Int, length:Int):Void
		java.lang.System.arraycopy(sourceArray, sourceIndex, destinationArray, destinationIndex, length);

	@:extern private static inline function assert(x:Bool):Void
	{
#if DEBUG_HASHTBL
		if (!x) throw "assert failed";
#end
	}
}

private typedef HashType = Int;
