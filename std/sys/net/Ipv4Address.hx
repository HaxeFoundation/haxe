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
import sys.net.IpAddress;

/**
	An IPv4 address, as defined in IETF RFC 791.
	This value is immutable.
**/
@:notNull
abstract Ipv4Address(Int) {
	public static final BROADCAST:Ipv4Address = new Ipv4Address(255, 255, 255, 255);
	public static final LOCALHOST:Ipv4Address = new Ipv4Address(127, 0, 0, 1);
	public static final ANY:Ipv4Address = new Ipv4Address(0, 0, 0, 0);

	/**
		Constructs a new IPv4 address from four octets.
	**/
	public inline function new(a:Int, b:Int, c:Int, d:Int) {
		var value:Int = 0;
		value |= (a & 255);
		value |= (b & 255) << 8;
		value |= (c & 255) << 16;
		value |= (d & 255) << 24;
		this = value;
	}

	/**
		Returns true if two IPv4 addresses are equal.
	**/
	@:op(A == B)
	public static inline function equals(lhs:Ipv4Address, rhs:Ipv4Address):Bool {
		return (cast lhs : Int) == (cast rhs : Int);
	}

	/**
		Returns true if two IPv4 addresses are not equal.
	**/
	@:op(A != B)
	public static inline function notEquals(lhs:Ipv4Address, rhs:Ipv4Address):Bool {
		return !equals(lhs, rhs);
	}

	/**
		Returns four octets that make up this address.
	**/
	public function octets():Vector<Int> {
		final v = new Vector<Int>(4);
		v[0] = this & 255;
		v[1] = (this >> 8) & 255;
		v[2] = (this >> 16) & 255;
		v[3] = (this >> 24) & 255;
		return v;
	}

	/**
		Returns true if this is a loopback address.
	**/
	public inline function isLoopback():Bool {
		final octets = abstract.octets();
		return octets[0] == 127;
	}

	/**
		Returns true if this is a link-local address.
	**/
	public inline function isLinkLocal():Bool {
		final octets = abstract.octets();
		return octets[0] == 169 && octets[1] == 254;
	}

	/**
		Returns true if this is a multicast address.
	**/
	public inline function isMulticast():Bool {
		final octets = abstract.octets();
		return octets[0] >= 224 && octets[0] <= 239;
	}

	/**
		Returns true if this address is unspecified.
	**/
	public inline function isUnspecified():Bool {
		return abstract == Ipv4Address.ANY;
	}

	/**
		Returns true if this is a broadcast address.
	**/
	public inline function isBroadcast():Bool {
		return abstract == Ipv4Address.BROADCAST;
	}

	@:to
	private inline function asIpAddress():IpAddress {
		return IpAddress.V4(abstract);
	}

	/**
		Gets a text representation of this IPv4 address.
	**/
	public function toString():String {
		final octets = abstract.octets();
		final sb = new StringBuf();
		sb.add(octets[0]);
		sb.addChar(".".code);
		sb.add(octets[1]);
		sb.addChar(".".code);
		sb.add(octets[2]);
		sb.addChar(".".code);
		sb.add(octets[3]);
		return sb.toString();
	}

	/**
		Tries to parse a string as an IPv4 address.

		@param str The string to parse.
		@return The parsed IPv4 address, or `null` if the string could not be parsed.
	**/
	public static function tryParse(str:String):Null<Ipv4Address> {
		final parts = StringTools.trim(str).split(".");
		if (parts.length != 4) {
			return null;
		}

		final octets:Array<Int> = [];
		for (part in parts) {
			static final patternDigit = ~/^\d{1,3}$/;
			if (!patternDigit.match(part)) {
				return null;
			}
			final octet = Std.parseInt(part);
			if (octet > 255) {
				return null;
			}
			octets.push(octet);
		}

		return new Ipv4Address(octets[0], octets[1], octets[2], octets[3]);
	}
}
