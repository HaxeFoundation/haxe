package unit.issues;

class Issue4203 extends Test {
  public function test() {
    var thing = new Thing1(true);
    t(thing.getIt());
    var ni:NativeItem<Bool> = thing;
    t(ni.getIt());
  }
}

#if java
@:nativeGen
#end
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
