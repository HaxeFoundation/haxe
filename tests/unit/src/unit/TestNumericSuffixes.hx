package unit;

class TestNumericSuffixes extends Test {
    
    public function testIntSuffixes() {
        eq(7i32, 7);
        eq(-7i32, -7);
        eq(-1u32, (-1 : UInt));
        eq(3000000000000i64 + "", "3000000000000");
        eq(9223372036854775807i64 + "", "9223372036854775807");
    }

    public function testFloatSuffixes() {
        eq(7.0f64, 7.0);
        eq(-7.0f64, -7.0);
        eq(1f64 is Float, true);
        eq(.0f64, 0.0);
        eq(7e+0f64, 7e+0);
        eq(7.0e+0f64, 7.0e+0);
    }

    public function testHexSuffixes() {
        eq(0xFFFFFFFFi32, -1);
        eq(0xFFFFFFFFu32, (0xFFFFFFFF : UInt));
        eq(0xFFFFFFFFi64 + "", "4294967295");
        eq(0xFFFFFFFFFFFFFFFFi64 + "", "-1");
        eq(0x7FFFFFFFFFFFFFFFi64 + "", "9223372036854775807");
    }
}