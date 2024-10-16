package net;

import sys.net.Ipv6Address;
import utest.Assert;
import utest.Test;

class TestIpv6Address extends Test {
	public function testAddressesAreComparedByValue() {
		Assert.isTrue(new Ipv6Address(0, 0, 0, 0, 0, 0, 0, 1) == Ipv6Address.LOCALHOST);
		Assert.isTrue(new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8) == new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8));
		Assert.isFalse(new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8) == new Ipv6Address(5, 6, 7, 8, 1, 2, 3, 4));
		Assert.isTrue(new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8) != new Ipv6Address(5, 6, 7, 8, 1, 2, 3, 4));
	}

	public function testAddressesAreCorrectlyStringified() {
		Assert.equals("::1", Ipv6Address.LOCALHOST.toString());
		Assert.equals("::", Ipv6Address.ANY.toString());
		Assert.equals("1:2:3:4:5:6:7:8", new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8).toString());
		Assert.equals("1:0:2:0:3:0:4:0", new Ipv6Address(1, 0, 2, 0, 3, 0, 4, 0).toString());
		Assert.equals("fe80::1:2", new Ipv6Address(0xfe80, 0, 0, 0, 0, 0, 0x1, 0x2).toString());
		Assert.equals("fe80::", new Ipv6Address(0xfe80, 0, 0, 0, 0, 0, 0, 0).toString());
		Assert.equals("10:0:0:20::30", new Ipv6Address(0x10, 0, 0, 0x20, 0, 0, 0, 0x30).toString());
		Assert.equals("10::20:0:0:30", new Ipv6Address(0x10, 0, 0, 0, 0x20, 0, 0, 0x30).toString());
	}

	public function testValidAddressesAreSuccessfullyParsed() {
		Assert.isTrue(new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8) == Ipv6Address.tryParse("1:2:3:4:5:6:7:8"));
		Assert.isTrue(new Ipv6Address(1, 2, 3, 4, 5, 6, 7, 8) == Ipv6Address.tryParse("01:02:03:04:05:06:07:08"));
		Assert.isTrue(new Ipv6Address(0, 0, 0, 0, 0, 0, 0, 1) == Ipv6Address.tryParse("::1"));
		Assert.isTrue(new Ipv6Address(0, 0, 0, 0, 0, 0, 0, 0) == Ipv6Address.tryParse("::"));
		Assert.isTrue(new Ipv6Address(1, 0, 2, 0, 0, 0, 3, 4) == Ipv6Address.tryParse("1:0:2::3:4"));
		Assert.isTrue(new Ipv6Address(0, 0, 0, 0, 0, 0, 0xab, 0xcd) == Ipv6Address.tryParse("::AB:CD"));
		Assert.isTrue(new Ipv6Address(0xf2, 0, 0, 0, 0, 0, 0, 0) == Ipv6Address.tryParse(" f2::    "));
		Assert.isTrue(new Ipv6Address(0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
			0xffff) == Ipv6Address.tryParse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"));
	}

	public function testInvalidAddressesAreNotParsed() {
		Assert.isNull(Ipv6Address.tryParse("1:2:3:4:5:6:7"));
		Assert.isNull(Ipv6Address.tryParse("1:2:3:4:5:6:7:8:9"));
		Assert.isNull(Ipv6Address.tryParse("1::2::3"));
		Assert.isNull(Ipv6Address.tryParse("0ffff::1"));
		Assert.isNull(Ipv6Address.tryParse("fffff::1"));
		Assert.isNull(Ipv6Address.tryParse("::-1"));

		// No name resolution
		Assert.isNull(Ipv6Address.tryParse("localhost"));

		// No IPv4
		Assert.isNull(Ipv6Address.tryParse("127.0.0.1"));

		// No masks
		Assert.isNull(Ipv6Address.tryParse("1:2:3:4:5:6:0:0/96"));

		// No dual addresses
		Assert.isNull(Ipv6Address.tryParse("::ffff:1.2.3.4"));
	}
}
