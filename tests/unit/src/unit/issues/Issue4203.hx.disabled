package unit.issues;

class Issue4203 extends Test {
	public function test() {
#if !cpp
		var val = new Thing1(true);
		eq(val.getIt(), true);
		t(val.getIt());
#end
	}
}

#if !cpp
@:nativeGen
private interface NativeItem<T> {
    public function getIt() : T;
}

private class Thing1 implements NativeItem<Bool> {
    var value: Bool;
    public function new(value) {
        this.value = value;
    }

    public function getIt() : Bool {
        return value;
    }
}
#end
