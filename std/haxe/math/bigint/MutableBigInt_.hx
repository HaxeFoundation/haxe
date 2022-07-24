/*
 * Copyright (C)2005-2022 Haxe Foundation
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
 
package haxe.math.bigint;

import haxe.math.bigint.BigIntExceptions;
import haxe.math.bigint.BigIntHelper;
import haxe.ds.Vector;
import haxe.io.Bytes;

@:allow(unit)
@:allow(haxe.math.bigint)
class MutableBigInt_ extends BigInt_ 
{
	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------

	/**
		Set the value of this big int with an integer of value `value`.
	**/
	public function setFromInt(value : Int) : Void
	{
		ensureCapacity(1, false);
		m_data.set(0, value);
		m_count = 1;
	}

	/**
		Set the value of this big integer with the signed value
		represented by the hexadecimal string `value`.
	**/
	public inline function setFromHexSigned(value : String) : Void
	{
		_setFromHex(value, true);
	}

	/**
		Set the value of this big integer with the unsigned value
		represented by the hexadecimal string `value`.
	**/
	public inline function setFromHexUnsigned(value : String) : Void
	{
		_setFromHex(value, false);
	}

	/**
		Set the value of this big integer with the value represented by the decimal string `value`.
	**/
	public function setFromString(value : String) : Void
	{
		if ((value == null) || (value.length < 1))
		{
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		var negate = value.charCodeAt(0) == 0x2d;
		var index = negate ? 1 : 0;
		if (value.length <= index)
		{
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		this.setFromInt(0);
		var t = new MutableBigInt_();
		for (i in index ... value.length)
		{
			var c = value.charCodeAt(i);
			if ((48 <= c) && (c <= 57))
			{
				BigIntArithmetic.multiplyInt(t, this, 10);
				BigIntArithmetic.addInt(this, t, c - 48);
			}
			else
			{
				throw BigIntExceptions.INVALID_ARGUMENT;
			}
		}
		if (negate)
		{
			BigIntArithmetic.negate(this, this);
		}
	}

	/**
		Set the value of this big integer with the unsigned value
		represented by the integer vector `value`.
	**/
	public function setFromUnsignedInts(value : Vector<Int>, length : Int = 0) : Void
	{
		if (length <= 0)
		{
			length = value.length;
		}
		var neg = value.get(length - 1) >>> 31;
		ensureCapacity(length + neg, false);
		m_data.set(length + neg - 1, 0);
		MultiwordArithmetic.copy(m_data, value, length);
		m_count = length + neg;
		compact();
	}
	
	public function setFromBigEndianBytesSigned(value:Bytes, offset:Int = 0, valueLength:Int = 0):Void 
	{
		if ( value == null) {
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		if (valueLength <= 0) {
			valueLength = value.length;
		}
		if (offset + valueLength > value.length) {
			throw BigIntExceptions.BUFFER_TOO_SMALL;
		}
		if (valueLength < 1) {
			setFromInt(0);
			return;
		}
		var length = (valueLength + 3) >> 2;
		ensureCapacity(length, false);
		m_data.set(length - 1, 0);
		var pos = 0;
		var i = offset + valueLength;
		while (i >= offset + 4) {
			m_data.set(pos++, (value.get(i - 1) << 0) | (value.get(i - 2) << 8) | (value.get(i - 3) << 16) | (value.get(i - 4) << 24));
			i -= 4;
		}
		if (i > offset) {
			var x:Int = 0;
			for (j in offset...i) {
				x = (x << 8) | value.get(j);
			}
			m_data.set(pos++, x);
		}
		m_count = length;
		compact();
	}

	public function setFromBigEndianBytesUnsigned(value : Bytes, offset : Int = 0, valueLength : Int = 0) : Void
	{
		if (valueLength <= 0)
		{
			valueLength = value.length;
		}
		if (offset + valueLength > value.length)
		{
			throw BigIntExceptions.BUFFER_TOO_SMALL;
		}
		if (valueLength < 1)
		{
			setFromInt(0);
			return;
		}
		var neg = ((valueLength & 3) == 0) ? (value.get(0) >> 7) : 0;
		var length = (valueLength + 3) >> 2;
		ensureCapacity(length + neg, false);
		m_data.set(length + neg - 1, 0);
		var pos = 0;
		var i = offset + valueLength;
		while (i >= offset + 4)
		{
			m_data.set(pos++,
				(value.get(i - 1) <<  0) |
				(value.get(i - 2) <<  8) |
				(value.get(i - 3) << 16) |
				(value.get(i - 4) << 24));
			i -= 4;
		}
		if (i > offset)
		{
			var x : Int = 0;
			for (j in offset ... i)
			{
				x = (x << 8) | value.get(j);
			}
			m_data.set(pos++, x);
		}
		m_count = length + neg;
		compact();
	}

	public function setFromLittleEndianBytesUnsigned(value : Bytes, offset : Int = 0, valueLength : Int = 0) : Void
	{
		if (valueLength <= 0)
		{
			valueLength = value.length;
		}
		if (offset + valueLength > value.length)
		{
			throw BigIntExceptions.BUFFER_TOO_SMALL;
		}
		if (valueLength < 1)
		{
			setFromInt(0);
			return;
		}
		var neg = ((valueLength & 3) == 0) ? (value.get(valueLength - 1) >> 7) : 0;
		var length = (valueLength + 3) >> 2;
		ensureCapacity(length + neg, false);
		m_data.set(length + neg - 1, 0);
		var pos = 0;
		var i = offset;
		while (i <= offset + valueLength - 4)
		{
			m_data.set(pos++,
				(value.get(i + 0) <<  0) |
				(value.get(i + 1) <<  8) |
				(value.get(i + 2) << 16) |
				(value.get(i + 3) << 24));
			i += 4;
		}
		if (i < offset + valueLength)
		{
			var x : Int = 0;
			for (j in i ... offset + valueLength)
			{
				x |= value.get(j) << ((j - i) << 3);
			}
			m_data.set(pos++, x);
		}
		m_count = length + neg;
		compact();
	}

	public function clear() : Void
	{
		MultiwordArithmetic.setZero(m_data, m_data.length);
		m_count = 1;
	}

	/**
		Copy the value from big integer `other` into this big
		integer.
	**/
	private function copyFrom(other : BigInt_) : Void
	{
		if (other != this)
		{
			ensureCapacity(other.m_count, false);
			for (i in 0 ... other.m_count)
			{
				m_data.set(i, other.m_data.get(i));
			}
			m_count = other.m_count;
		}
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	private inline function setShort(n : Int32, v : Int32) : Void
	{
		var s : Int = (n & 1) << 4;
		var t : Int = m_data.get(n >> 1) & (~0xffff >>> s);
		m_data.set(n >> 1, t | ((v & 0xffff) << s));
	}

	private function copy(other : MutableBigInt_) : Void
	{
		this.m_data = other.m_data;
		this.m_count = other.m_count;
		this.m_owned = other.m_owned;
	}

	private inline function ensureCapacity(n : Int, preserve : Bool) : Void
	{
		#if debug
			if (s_testAllocation)
			{
				ensureCapacityDebug(n, preserve);
				return;
			}
		#end
		ensureCapacityProd(n, preserve);
	}

	@:noCompletion
	private function ensureCapacityDebug(n : Int, preserve : Bool) : Void
	{
		// always allocate the minimum amount necessary, to catch
		// bounds edge cases as well as use of stale buffer data
		if (preserve && (m_data != null) && (m_count > 0))
		{
			n = (m_count > n) ? m_count : n;
			n += s_debugAllocationPadding;
			var newData = new Vector<Int>(n);
			for (i in 0 ... m_count)
			{
				newData.set(i, m_data.get(i));
			}
			for (i in m_count ... n)
			{
				newData.set(i, 0xdeadbeef);
			}
			m_data = newData;
		}
		else
		{
			n += s_debugAllocationPadding;
			m_data = new Vector<Int>(n);
			for (i in 0 ... n)
			{
				m_data.set(i, 0xdeadbeef);
			}
		}
	}

	@:noCompletion
	private function ensureCapacityProd(n : Int, preserve : Bool) : Void
	{
		if (n < 1)
		{
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		if ((!m_owned) || (m_data == null) || (n > m_data.length))
		{
			n = BigIntHelper.clp2(n);
			if (preserve && (m_data != null))
			{
				var newData = new Vector<Int>(n);
				for (i in 0 ... m_count)
				{
					newData.set(i, m_data.get(i));
				}
				m_data = newData;
			}
			else
			{
				m_data = new Vector<Int>(n);
			}
		}
		m_owned = true;
	}

	private function new()
	{
		super();
	}

	private static function fromInt(other : Int) : MutableBigInt_
	{
		var c = BigInt_.getCachedValue(other);
		if (c != null)
		{
			return fromBigInt(c);
		}
		var r = new MutableBigInt_();
		r.ensureCapacity(1, false);
		r.m_data.set(0, other);
		r.m_count = 1;
		return r;
	}

	private static function fromBigInt(other : BigInt_) : MutableBigInt_
	{
		// TODO: this will be problematic if `other` is actually a MutableBigInt_
		var r = new MutableBigInt_();	// unowned
		r.m_data = other.m_data;
		r.m_count = other.m_count;
		return r;
	}

	private var m_owned : Bool = false;

	private static var s_testAllocation : Bool = false;
	private static var s_debugAllocationPadding : Int = 0;

	//-----------------------------------------------------------------------
	// Static helpers
	//-----------------------------------------------------------------------

	private function _setFromHex(value : String, signed : Bool) : Void
	{
		if (value == null)
		{
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		var index = value.length;
		if (index <= 0)
		{
			throw BigIntExceptions.INVALID_ARGUMENT;
		}
		var extra : Int = signed ? 0 : 1;
		ensureCapacity(((index + 7) >> 3) + extra, false);
		var pos = -1;
		var bit : Int = 32;
		var c : Int32 = 0;
		while (index > 0)
		{
			c = value.charCodeAt(--index);
			if ((48 <= c) && (c <= 57))
			{
				c -= 48;
			}
			else if ((65 <= c) && (c <= 70))
			{
				c -= 55;
			}
			else if ((97 <= c) && (c <= 102))
			{
				c -= 87;
			}
			else if (c == 32)
			{
				continue;
			}
			else
			{
				throw BigIntExceptions.INVALID_ARGUMENT;
			}
			if (bit >= 32)
			{
				m_data.set(++pos, 0);
				bit = 0;
			}
			m_data.set(pos, m_data.get(pos) | (c << bit));
			bit += 4;
		}
		// Sign extend
		m_count = pos + 1;
		if (signed)
		{
			c = ((c & 8) != 0) ? 15 : 0;
			while (bit < 32)
			{
				m_data.set(pos, m_data.get(pos) | (c << bit));
				bit += 4;
			}
		}
		else if (m_data.get(pos) < 0)
		{
			m_data.set(m_count++, 0);
		}
		compact();
	}

	@:noCompletion
	private static inline function multiplyAssignInt2(a : MutableBigInt_, b : Int) : Void
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.multiplyInt(r, a, b);
		a.copy(r);
	}

	@:noCompletion
	private static inline function multiplyAssign2(a : MutableBigInt_, b : BigInt_) : Void
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.multiply(r, a, b);
		a.copy(r);
	}

	@:noCompletion
	private static inline function divideAssignInt2(a : MutableBigInt_, b : Int) : Void
	{
		var q = new MutableBigInt_();
		BigIntArithmetic.divideInt(a, b, q);
		a.copy(q);
	}

	@:noCompletion
	private static inline function divideAssign2(a : MutableBigInt_, b : BigInt_) : Void
	{
		var q = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, null);
		a.copy(q);
	}

	@:noCompletion
	private static inline function modulusAssignInt2(a : MutableBigInt_, b : Int) : Void
	{
		var q = new MutableBigInt_();
		var r = BigIntArithmetic.divideInt(a, b, q);
		a.setFromInt(r);
	}

	@:noCompletion
	private static inline function modulusAssign2(a : MutableBigInt_, b : BigInt_) : Void
	{
		var q = new MutableBigInt_();
		var r = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, r);
		a.copy(r);
	}

	@:noCompletion
	private static inline function arithmeticShiftLeftAssign2(a : MutableBigInt_, b : Int) : Void
	{
		BigIntArithmetic.arithmeticShiftLeft(a, a, b);
	}

	@:noCompletion
	private static inline function arithmeticShiftRightAssign2(a : MutableBigInt_, b : Int) : Void
	{
		BigIntArithmetic.arithmeticShiftRight(a, a, b);
	}
}
