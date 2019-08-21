package unit.issues;

private enum E {
	A;
	B;
}

private enum MPEGVersion {
	MPEG_Reserved;
}

private typedef MP3Header = {
	public var version : MPEGVersion;
}

class Issue5031 extends Test {
	static function getE() {
		return A;
	}

	function testHscriptProblem() {
		while (true) {
			switch (getE()) {
				case A:
					break;
				case B:
			}
		}
		noAssert();
	}

	function isInvalidFrameHeader(hdr:MP3Header) {
		return hdr.version != MPEG_Reserved;
	}

	function testFormatProblem() {
		f(isInvalidFrameHeader({version: MPEG_Reserved}));
	}
}