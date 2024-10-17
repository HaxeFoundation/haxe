package net;

import sys.net.Ipv4Address;
import utest.Assert;
import utest.Test;

class TestIpv4Address extends Test {
	public function testAddressesAreComparedByValue() {
		Assert.isTrue(new Ipv4Address(127, 0, 0, 1) == Ipv4Address.LOCALHOST);
		Assert.isTrue(new Ipv4Address(1, 2, 3, 4) == new Ipv4Address(1, 2, 3, 4));
		Assert.isFalse(new Ipv4Address(1, 2, 3, 4) == new Ipv4Address(5, 6, 7, 8));
		Assert.isTrue(new Ipv4Address(1, 2, 3, 4) != new Ipv4Address(5, 6, 7, 8));
	}

	public function testAddressesAreCorrectlyStringified() {
		Assert.equals("127.0.0.1", Ipv4Address.LOCALHOST.toString());
		Assert.equals("1.2.3.4", new Ipv4Address(1, 2, 3, 4).toString());
	}

	public function testValidAddressesAreSuccessfullyParsed() {
		Assert.isTrue(new Ipv4Address(0, 0, 0, 0) == Ipv4Address.tryParse("0.0.0.0"));
		Assert.isTrue(new Ipv4Address(1, 2, 3, 4) == Ipv4Address.tryParse("1.2.3.4"));
		Assert.isTrue(new Ipv4Address(1, 1, 1, 1) == Ipv4Address.tryParse("  1.1.1.1 "));
		Assert.isTrue(new Ipv4Address(255, 255, 255, 255) == Ipv4Address.tryParse("255.255.255.255"));
	}

	public function testInvalidAddressesAreNotParsed() {
		Assert.isNull(Ipv4Address.tryParse("0"));
		Assert.isNull(Ipv4Address.tryParse("0.0"));
		Assert.isNull(Ipv4Address.tryParse("0.0.0"));
		Assert.isNull(Ipv4Address.tryParse("0.0.0.0.0"));
		Assert.isNull(Ipv4Address.tryParse("-1.-2.-3.-4"));
		Assert.isNull(Ipv4Address.tryParse("256.256.256.256"));

		// No name resolution
		Assert.isNull(Ipv4Address.tryParse("localhost"));

		// No IPv6
		Assert.isNull(Ipv4Address.tryParse("::1"));

		// No masks
		Assert.isNull(Ipv4Address.tryParse("192.180.5.0/24"));

		// No alternative notations
		Assert.isNull(Ipv4Address.tryParse("2147483775"));
		Assert.isNull(Ipv4Address.tryParse("00110110101010101010111111010010"));
		Assert.isNull(Ipv4Address.tryParse("00110110 10101010 10101111 11010010"));
		Assert.isNull(Ipv4Address.tryParse("af.c0.5.b"));
		Assert.isNull(Ipv4Address.tryParse("c56701a3"));
		Assert.isNull(Ipv4Address.tryParse("0xc56701a3"));
	}

	public function testAddressesAreConvertedToNetworkOrder() {
		Assert.equals(0x0100007f, Ipv4Address.LOCALHOST.asNetworkOrderInt());
	}

	public function testAddressesAreConvertedFromNetworkOrder() {
		Assert.equals("127.0.0.1", Ipv4Address.fromNetworkOrderInt(0x0100007f).toString());
	}
}
