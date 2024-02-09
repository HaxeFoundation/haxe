package unit;

class TestNumericSeparator extends Test {
	public function test() {
		// normal int
		eq(12_0, 120);
		eq(1_2_0, 120);

		// hex int
		eq(0x12_0, 0x120);
		eq(0x1_2_0, 0x120);

		// normal float
		feq(12.3_4, 12.34);
		feq(1_2.34, 12.34);
		feq(1_2.3_4, 12.34);

		// dot float
		feq(.3_4, .34);
		feq(.3_4_5, .345);

		// science float
		feq(1_2e3_4, 12e34);
		feq(1_2.3e4_5, 12.3e45);

		// int but actually float
		feq(1_2f64, 12f64);
	}

	public function testWithSuffix() {
		// normal int
		eq(12_0i32, 120i32);
		eq(1_2_0i32, 120i32);

		// hex int
		eq(0x12_0i32, 0x120i32);
		eq(0x1_2_0i32, 0x120i32);

		// normal float
		feq(12.3_4f64, 12.34f64);
		feq(1_2.34f64, 12.34f64);
		feq(1_2.3_4f64, 12.34f64);

		// dot float
		feq(.3_4f64, .34f64);
		feq(.3_4_5f64, .345f64);

		// science float
		feq(1_2e3_4f64, 12e34f64);
		feq(1_2.3e4_5f64, 12.3e45f64);
	}

	public function testJustBeforeSuffix() {
		// normal int
		eq(12_0_i32, 120i32);
		eq(1_2_0_i32, 120i32);

		// hex int
		eq(0x12_0_i32, 0x120i32);
		eq(0x1_2_0_i32, 0x120i32);

		// normal float
		feq(12.3_4_f64, 12.34f64);
		feq(1_2.34_f64, 12.34f64);
		feq(1_2.3_4_f64, 12.34f64);

		// dot float
		feq(.3_4_f64, .34f64);
		feq(.3_4_5_f64, .345f64);

		// science float
		feq(1_2e3_4_f64, 12e34f64);
		feq(1_2.3e4_5_f64, 12.3e45f64);
	}
}
