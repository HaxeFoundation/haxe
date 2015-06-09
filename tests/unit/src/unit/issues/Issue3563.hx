package unit.issues;

private class FinalTest {
	public static var hello = "Hello";
	public static var someValue = getSomeValue();
	public static var someValue2 = getSomeValue2();

	static public function getSomeValue() {
		return hello;
	}

    static public function getSomeValue2() {
        return someValue;
    }
}

class Issue3563 extends Test {
	function test() {
		eq("Hello", FinalTest.someValue);
		eq("Hello", FinalTest.getSomeValue());
		eq("Hello", FinalTest.getSomeValue2());
	}
}