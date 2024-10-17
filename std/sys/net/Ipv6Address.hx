/*
 * Copyright (C)2005-2024 Haxe Foundation
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

package sys.net;

import haxe.ds.Vector;
import haxe.exceptions.ArgumentException;
import haxe.io.Bytes;
import haxe.io.BytesData;

/**
	An IPv6 address, as defined in IETF RFC 4291.
	This value is immutable.
**/
@:notNull
abstract Ipv6Address(Ipv6AddressImpl) {
	public static final LOCALHOST:Ipv6Address = new Ipv6Address(0, 0, 0, 0, 0, 0, 0, 1);
	public static final ANY:Ipv6Address = new Ipv6Address(0, 0, 0, 0, 0, 0, 0, 0);

	/**
		Constructs a new IPv6 address from eight 16-bit groups.
	**/
	public inline function new(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int) {
		this = Ipv6AddressImpl.fromGroups(a, b, c, d, e, f, g, h);
	}

	/**
		Returns true if two IPv6 addresses are equal.
	**/
	@:op(A == B)
	public static inline function equals(lhs:Ipv6Address, rhs:Ipv6Address):Bool {
		return Ipv6AddressImpl.equals(cast lhs, cast rhs);
	}

	/**
		Returns true if two IPv6 addresses are not equal.
	**/
	@:op(A != B)
	public static inline function notEquals(lhs:Ipv6Address, rhs:Ipv6Address):Bool {
		return !equals(lhs, rhs);
	}

	/**
		Returns true if this is a loopback address.
	**/
	public inline function isLoopback():Bool {
		return abstract == Ipv6Address.LOCALHOST;
	}

	/**
		Returns true if this IPv6 address is an IPv4-mapped address.
	**/
	public inline function isIpv4Mapped():Bool {
		return this.isIpv4Mapped();
	}

	/**
		Returns a text, lowercase hexadecimal representation of this IPv6 address.
	**/
	public inline function toString():String {
		return this.toString();
	}

	@:to
	private inline function asIpAddress():IpAddress {
		return IpAddress.V6(abstract);
	}

	/**
		Returns this IPv6 address represented as big-endian bytes.
	**/
	private inline function asNetworkOrderBytes():BytesData {
		return @:privateAccess this.repr;
	}

	/**
		Creates a new IPv6 address object from big-endian bytes.
	**/
	private static inline function fromNetworkOrderBytes(b:BytesData):Ipv6Address {
		return cast new Ipv6AddressImpl(b);
	}

	/**
		Tries to parse a string as an IPv6 address.

		@param str The string to parse.
		@return The parsed IPv6 address, or `null` if the string could not be parsed.
	**/
	public static inline function tryParse(str:String):Null<Ipv6Address> {
		return cast Ipv6AddressImpl.tryParse(str);
	}
}

@:noDoc
private final class Ipv6AddressImpl {
	/**
		Inner representation of the IPv6 address.
	**/
	private final repr:BytesData;

	public function new(repr:BytesData) {
		this.repr = repr;
	}

	private function groups():Vector<Int> {
		final bytes = Bytes.ofData(this.repr);
		final v = new Vector<Int>(8);
		v[0] = bytes.getUInt16(14);
		v[1] = bytes.getUInt16(12);
		v[2] = bytes.getUInt16(10);
		v[3] = bytes.getUInt16(8);
		v[4] = bytes.getUInt16(6);
		v[5] = bytes.getUInt16(4);
		v[6] = bytes.getUInt16(0);
		v[7] = bytes.getUInt16(2);
		return v;
	}

	public function isIpv4Mapped():Bool {
		final groups = this.groups();
		if (groups[0] != 0)
			return false;
		if (groups[1] != 0)
			return false;
		if (groups[2] != 0)
			return false;
		if (groups[3] != 0)
			return false;
		if (groups[4] != 0)
			return false;
		return groups[5] == 0xffff;
	}

	public function toString():String {
		final groups = this.groups();

		// Detect longest run of zeros
		var firstZeroAt:Null<Int> = null;
		var zerosCount:Int = 0;
		{
			var currentStart:Null<Int> = null;
			var currentLen:Int = 0;
			for (i in 0...8) {
				if (groups[i] == 0) {
					if (currentStart == null) {
						currentStart = i;
					}
					currentLen += 1;
					if (currentLen > zerosCount) {
						zerosCount = currentLen;
						firstZeroAt = currentStart;
					}
				} else {
					currentStart = null;
					currentLen = 0;
				}
			}
		}

		final sb = new StringBuf();

		if (zerosCount >= 2) {
			for (i in 0...firstZeroAt) {
				sb.add(StringTools.hex(groups[i]));
				if (i + 1 < firstZeroAt) {
					sb.addChar(":".code);
				}
			}
			sb.add("::");
			for (j in (firstZeroAt + zerosCount)...8) {
				sb.add(StringTools.hex(groups[j]));
				if (j < 7) {
					sb.addChar(":".code);
				}
			}
		} else {
			for (i in 0...8) {
				sb.add(StringTools.hex(groups[i]));
				if (i < 7) {
					sb.addChar(":".code);
				}
			}
		}

		return sb.toString().toLowerCase();
	}

	public static function equals(lhs:Ipv6AddressImpl, rhs:Ipv6AddressImpl):Bool {
		return Bytes.ofData(lhs.repr).compare(Bytes.ofData(rhs.repr)) == 0;
	}

	public static function fromGroups(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int, h:Int):Ipv6AddressImpl {
		final bytes = Bytes.alloc(16);
		bytes.setUInt16(14, a);
		bytes.setUInt16(12, b);
		bytes.setUInt16(10, c);
		bytes.setUInt16(8, d);
		bytes.setUInt16(6, e);
		bytes.setUInt16(4, f);
		bytes.setUInt16(0, g);
		bytes.setUInt16(2, h);
		return new Ipv6AddressImpl(bytes.getData());
	}

	public static function fromGroupsVector(groups:Vector<Int>):Ipv6AddressImpl {
		if (groups.length != 8) {
			throw new ArgumentException("Groups do not represent an IPv6 address");
		}

		final a = groups[0];
		final b = groups[1];
		final c = groups[2];
		final d = groups[3];
		final e = groups[4];
		final f = groups[5];
		final g = groups[6];
		final h = groups[7];

		return Ipv6AddressImpl.fromGroups(a, b, c, d, e, f, g, h);
	}

	public static function tryParse(str:String):Null<Ipv6AddressImpl> {
		static final patternHex = ~/^[0-9a-f]{1,4}$/;

		final parts = StringTools.trim(str).toLowerCase().split(":");
		if (parts.length < 3 || parts.length > 8) {
			return null;
		}

		final groups = new Vector<Int>(8);
		groups.fill(0);

		var i:Int = 0;
		while (i < 8) {
			if (i >= parts.length) {
				return null;
			}

			final part = parts[i];
			if (part == "") {
				break;
			}

			if (!patternHex.match(part)) {
				return null;
			}

			groups[i] = Std.parseInt('0x$part');
			i++;
		}

		var j:Int = 0;
		while ((parts.length - 1 - j) > i) {
			final part = parts[(parts.length - 1 - j)];
			if (part == "") {
				if ((i == 0 && j == 0) || (i + j + 2 == parts.length)) {
					break;
				} else {
					return null;
				}
			}

			if (!patternHex.match(part)) {
				return null;
			}

			groups[7 - j] = Std.parseInt('0x$part');
			j++;
		}

		return Ipv6AddressImpl.fromGroupsVector(groups);
	}
}
