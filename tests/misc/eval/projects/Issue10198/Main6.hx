class Main6 {
	static function fn2<T, R:T>(r:R, fn:()->T):T
		return null;

	static function test() {
		var a:Parent = fn2((null:Child), () -> new GrandParent());
	}

}

private class GrandParent {
	public function new() {}
}
private class Parent extends GrandParent {}
private class Child extends Parent {}