package test;

import haxe.io.Bytes;
import asys.FilePermissions;

using asys.net.AddressTools;

class TestMisc extends Test {
	/**
		Tests `sys.FilePermissions`. No actual system calls are tested here; see
		e.g. `TestFileSystem.testAccess`.
	**/
	function testFilePermissions() {
		eq(("---------" : FilePermissions), None);
		eq(("r--------" : FilePermissions), ReadOwner);
		eq(("-w-------" : FilePermissions), WriteOwner);
		eq(("--x------" : FilePermissions), ExecuteOwner);
		eq(("---r-----" : FilePermissions), ReadGroup);
		eq(("----w----" : FilePermissions), WriteGroup);
		eq(("-----x---" : FilePermissions), ExecuteGroup);
		eq(("------r--" : FilePermissions), ReadOthers);
		eq(("-------w-" : FilePermissions), WriteOthers);
		eq(("--------x" : FilePermissions), ExecuteOthers);
		eq(("rwx------" : FilePermissions), ReadOwner | WriteOwner | ExecuteOwner);
		eq(("---rwx---" : FilePermissions), ReadGroup | WriteGroup | ExecuteGroup);
		eq(("------rwx" : FilePermissions), ReadOthers | WriteOthers | ExecuteOthers);
		eq(("rw-rw-rw-" : FilePermissions), ReadOwner | WriteOwner | ReadGroup | WriteGroup | ReadOthers | WriteOthers);

		eq(ReadOwner, FilePermissions.fromOctal("400"));
		eq(ReadOwner | WriteOwner | ExecuteOwner, FilePermissions.fromOctal("700"));
		eq(ReadOwner | WriteOwner | ReadGroup | WriteGroup | ReadOthers | WriteOthers, FilePermissions.fromOctal("666"));
	}

	function testAddressTools() {
		f("127.256.0.1".isIpv4());
		f("127.0.1".isIpv4());

		f("::1::".isIpv6());
		f("1::2::3".isIpv6());
		f("1::127.0.1".isIpv6());
		f("::127.0.0.1:ffff:127.0.0.1".isIpv6());
		f("1234:1234:1234:1234::1234:1234:1234:1234".isIpv6());

		t("0.0.0.0".toIpv4().match(Ipv4(0)));
		t("255.255.255.255".toIpv4().match(Ipv4(0xFFFFFFFF)));
		t("127.0.0.1".toIpv4().match(Ipv4(0x7F000001)));
		t("123.32.1.0".toIpv4().match(Ipv4(0x7B200100)));

		"::".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("00000000000000000000000000000000")) => _));
		"::1".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("00000000000000000000000000000001")) => _));
		"1::".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("00010000000000000000000000000000")) => _));
		"2001:db8:1234:5678:11:2233:4455:6677".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("20010DB8123456780011223344556677")) => _));
		"4861:7865:2069:7320:6177:6573:6F6D:6521".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("4861786520697320617765736F6D6521")) => _));
		"1:2:3::ffff:127.0.0.1".toIpv6().match(Ipv6(beq(_, Bytes.ofHex("00010002000300000000FFFF7F000001")) => _));

		t("123.32.1.0".toIp().match(Ipv4(0x7B200100)));
		"123.32.1.0".toIp().mapToIpv6().match(Ipv6(beq(_, Bytes.ofHex("00000000000000000000FFFF7B200100")) => _));
		"1:2:3::ffff:127.0.0.1".toIp().match(Ipv6(beq(_, Bytes.ofHex("00010002000300000000FFFF7F000001")) => _));
	}
}
