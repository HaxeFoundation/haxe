import haxe.io.Bytes;

class TestConstants {
	// contents of resources-ro/hello.txt
	public static var helloString = "hello world
symbols ◊†¶•¬
non-BMP 🐄";
	public static var helloBytes = Bytes.ofString(helloString);

	// contents of resources-ro/binary.bin
	// - contains invalid Unicode, should not be used as string
	public static var binaryBytes = Bytes.ofHex("5554462D3820686572652C20627574207468656E3A2000FFFAFAFAFAF2F2F2F2F200C2A0CCD880E2ED9FBFEDA0800D0A");
}
