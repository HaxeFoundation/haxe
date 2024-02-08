package scripthost;
#if cpp

@:keep class Issue8502 {
	public function new() {
		//initialize variables
	}

	public function doTest1(f:cpp.Float32):String {
		return '$f';
	}

	public function doTest2(f:cpp.Float64):String {
		return '$f';
	}

	public function doTest3(f:cpp.Int8):String {
		return '$f';
	}

	public function doTest4(f:cpp.Int16):String {
		return '$f';
	}

	public function doTest5(f:cpp.Int32):String {
		return '$f';
	}

	public function doTest3u(f:cpp.UInt8):String {
		return '$f';
	}

	public function doTest4u(f:cpp.UInt16):String {
		return '$f';
	}

	public function doTest5u(f:cpp.UInt32):String {
		return '$f';
	}
}

#end