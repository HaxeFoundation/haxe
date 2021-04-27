class Main7 {
	static function fn2<T, R:T>(r:R):T
		return null;

	static function main() {
		var a:GrandParent = fn2((null:Child));
	}

}

private class GrandParent {
	public function new() {}
}
private class Parent extends GrandParent {}
private class Child extends Parent {}