package test;

import haxe.io.Bytes;
import utest.Async;

class TestDns extends Test {
	function testLocalhost(async:Async) {
		sub(async, done -> asys.net.Dns.lookup("localhost", {family: Ipv4}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv4(0x7F000001)));
			done();
		}));

		TestBase.uvRun();
	}

	function testIpv4(async:Async) {
		sub(async, done -> asys.net.Dns.lookup("127.0.0.1", {family: Ipv4}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv4(0x7F000001)));
			done();
		}));
		sub(async, done -> asys.net.Dns.lookup("123.32.10.1", {family: Ipv4}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv4(0x7B200A01)));
			done();
		}));
		sub(async, done -> asys.net.Dns.lookup("255.255.255.255", {family: Ipv4}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv4(0xFFFFFFFF)));
			done();
		}));

		TestBase.uvRun();
	}

	function testIpv6(async:Async) {
		sub(async, done -> asys.net.Dns.lookup("::1", {family: Ipv6}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv6(beq(_, Bytes.ofHex("00000000000000000000000000000001")) => _)));
			done();
		}));
		sub(async, done -> asys.net.Dns.lookup("2001:db8:1234:5678:11:2233:4455:6677", {family: Ipv6}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv6(beq(_, Bytes.ofHex("20010DB8123456780011223344556677")) => _)));
			done();
		}));
		sub(async, done -> asys.net.Dns.lookup("4861:7865:2069:7320:6177:6573:6F6D:6521", {family: Ipv6}, (err, res) -> {
			eq(err, null);
			t(res[0].match(Ipv6(beq(_, Bytes.ofHex("4861786520697320617765736F6D6521")) => _)));
			done();
		}));

		TestBase.uvRun();
	}
}
