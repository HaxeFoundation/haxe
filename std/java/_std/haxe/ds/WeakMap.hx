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
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

@:coreApi class WeakMap<K:{}, V> implements haxe.Constraints.IMap<K,V>
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
	private var entries:NativeArray<Entry<K,V>>;

  //weak map specific
	private var queue:ReferenceQueue<K>;

	private var nBuckets:Int;
	private var size:Int;
	private var nOccupied:Int;
	private var upperBound:Int;

	private var cachedEntry:Entry<K,V>;
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
		queue = new ReferenceQueue();
	}

	@:analyzer(ignore)
	private function cleanupRefs():Void
	{
		var x:Dynamic = null, nOccupied = nOccupied;
		while (( x = queue.poll()) != null)
		{
			//even if not found on hashtable (already removed), release value
			var x:Entry<K,V> = cast x;
			x.value = null;

			//lookup index
			if (nOccupied != 0)
			{
				var mask = nBuckets - 1, hash = x.hash, nProbes = 0;
				var i = hash & mask;
				var last = i, flag;
				while(!isEmpty(flag = hashes[i]) && (isDel(flag) || flag != hash || entries[i] != x))
				{
					i = (i + ++nProbes) & mask;
				}

				if (entries[i] == x)
				{
					if (cachedIndex == i)
					{
						cachedIndex = -1;
						cachedEntry = null;
					}
					entries[i] = null;
					hashes[i] = FLAG_DEL;
					--size;
				}
			}
		}
	}

	public function set( key : K, value : V ) : Void
	{
		cleanupRefs();
		var x:Int, k:Int;
		if (nOccupied >= upperBound)
		{
			if (nBuckets > (size << 1))
				resize(nBuckets - 1); //clear "deleted" elements
			else
				resize(nBuckets + 2);
		}

		k = hash(key);
		var hashes = hashes, entries = entries;
		{
			var mask = (nBuckets == 0) ? 0 : nBuckets - 1;
			var site = x = nBuckets;
			var i = k & mask, nProbes = 0;

			var delKey = -1;
			//for speed up
			if (isEmpty(hashes[i])) {
				x = i;
			} else {
				//var inc = getInc(k, mask);
				var last = i, flag;
				while(! (isEmpty(flag = hashes[i]) || (flag == k && entries[i].keyEquals(key) )) )
				{
					if (delKey == -1 && isDel(flag))
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

		var flag = hashes[x], entry = new Entry(key,value,k,queue);
		if (isEmpty(flag))
		{
			entries[x] = entry;
			hashes[x] = k;
			size++;
			nOccupied++;
		} else if (isDel(flag)) {
			entries[x] = entry;
			hashes[x] = k;
			size++;
		} else {
			assert(entries[x].keyEquals(key));
			entries[x] = entry;
		}

		cachedIndex = x;
		cachedEntry = entry;
	}

	@:final private function lookup( key : K ) : Int
	{
		if (nBuckets != 0)
		{
			var hashes = hashes, entries = entries;

			var mask = nBuckets - 1, hash = hash(key), k = hash, nProbes = 0;
			var i = k & mask;
			var last = i, flag;
			//var inc = getInc(k, mask);
			while (!isEmpty(flag = hashes[i]) && (isDel(flag) || flag != k || !entries[i].keyEquals(key)))
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
					var e = new NativeArray(newNBuckets);
					if (entries != null)
						arrayCopy(entries, 0, e, 0, nBuckets);
					entries = e;
				} //otherwise shrink
			}
		}

		if (j != 0)
		{ //rehashing is required
			//resetting cache
			cachedEntry = null;
			cachedIndex = -1;

			j = -1;
			var nBuckets = nBuckets, entries = entries, hashes = hashes;

			var newMask = newNBuckets - 1;
			while (++j < nBuckets)
			{
				var k;
				if (!isEither(k = hashes[j]))
				{
					var entry = entries[j];

					hashes[j] = FLAG_DEL;
					while (true) /* kick-out process; sort of like in Cuckoo hashing */
					{
						var nProbes = 0;
						var i = k & newMask;

						while (!isEmpty(newHash[i]))
							i = (i + ++nProbes) & newMask;

						newHash[i] = k;

						if (i < nBuckets && !isEither(k = hashes[i])) /* kick out the existing element */
						{
							{
								var tmp = entries[i];
								entries[i] = entry;
								entry = tmp;
							}

							hashes[i] = FLAG_DEL; /* mark it as deleted in the old hash table */
						} else { /* write the element and jump out of the loop */
							entries[i] = entry;
							break;
						}
					}
				}
			}

			if (nBuckets > newNBuckets) /* shrink the hash table */
			{
				{
					var e = new NativeArray(newNBuckets);
					arrayCopy(entries, 0, e, 0, newNBuckets);
					this.entries = e;
				}
			}

			this.hashes = newHash;
			this.nBuckets = newNBuckets;
			this.nOccupied = size;
			this.upperBound = Std.int(newNBuckets * HASH_UPPER + .5);
		}
	}

	public function get( key : K ) : Null<V>
	{
		cleanupRefs();
		var idx = -1;
		if (cachedEntry != null && cachedEntry.keyEquals(key) && ( (idx = cachedIndex) != -1 ))
		{
			return cachedEntry.value;
		}

		idx = lookup(key);
		if (idx != -1)
		{
			var entry = entries[idx];
			cachedEntry = entry;
			cachedIndex = idx;

			return entry.value;
		}

		return null;
	}

	private function getDefault( key : K, def : V ) : V
	{
		cleanupRefs();
		var idx = -1;
		if (cachedEntry != null && cachedEntry.keyEquals(key) && ( (idx = cachedIndex) != -1 ))
		{
			return cachedEntry.value;
		}

		idx = lookup(key);
		if (idx != -1)
		{
			var entry = entries[idx];
			cachedEntry = entry;
			cachedIndex = idx;

			return entry.value;
		}

		return def;
	}

	public function exists( key : K ) : Bool
	{
		cleanupRefs();
		var idx = -1;
		if (cachedEntry != null && cachedEntry.keyEquals(key) && ( (idx = cachedIndex) != -1 ))
		{
			return true;
		}

		idx = lookup(key);
		if (idx != -1)
		{
			var entry = entries[idx];
			cachedEntry = entry;
			cachedIndex = idx;

			return true;
		}

		return false;
	}

	public function remove( key : K ) : Bool
	{
		cleanupRefs();
		var idx = -1;
		if ( !(cachedEntry != null && cachedEntry.keyEquals(key) && ( (idx = cachedIndex) != -1 )) )
		{
			idx = lookup(key);
		}

		if (idx == -1)
		{
			return false;
		} else {
			if (cachedEntry != null && cachedEntry.keyEquals(key))
			{
				cachedIndex = -1;
				cachedEntry = null;
			}

			hashes[idx] = FLAG_DEL;
			entries[idx] = null;
			--size;

			return true;
		}
	}

	/**
		Returns an iterator of all keys in the hashtable.
		Implementation detail: Do not set() any new value while iterating, as it may cause a resize, which will break iteration
	**/
	public function keys() : Iterator<K>
	{
		cleanupRefs();
		var i = 0;
		var len = nBuckets;
		var lastKey = null; //keep a strong reference to the key while iterating, so it can't be collected while iterating
		return {
			hasNext: function() {
				for (j in i...len)
				{
					if (!isEither(hashes[j]))
					{
						var entry = entries[j];
						var last = entry.get();
						if (last != null)
						{
							lastKey = last;
							cachedIndex = i;
							cachedEntry = entry;
							i = j;
							return true;
						}
					}
				}
				return false;
			},
			next: function() {
				i = i + 1;
				return lastKey;
			}
		};
	}

	/**
		Returns an iterator of all values in the hashtable.
		Implementation detail: Do not set() any new value while iterating, as it may cause a resize, which will break iteration
	**/
	public function iterator() : Iterator<V>
	{
		cleanupRefs();
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
				var ret = entries[i].value;
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
			s.add(Std.string(i));
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
	@:extern private static inline function hash(s:Dynamic):HashType
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

private class Entry<K,V> extends WeakReference<K>
{
	public var value:V;
	public var hash(default, null):Int;
	public function new(key:K, value:V, hash:Int, queue:ReferenceQueue<K>)
	{
		super(key, queue);
		this.value = value;
		this.hash = hash;
	}

	@:final inline public function keyEquals(k:K):Bool
	{
		return k != null && untyped k.equals(get());
	}
}

private typedef HashType = Int;
