package unit.issues;

private class GenericHolder<T> {
    public var value:T;
    public function new() {}
}

private class ClassA {
    public function new() {}
    public function doSomething(x:Int) {}
}

private class ClassB {
    public function new() {}
    public function doSomethingElse(x:Int) {}
}

class Issue12872 extends Test {
	function test() {
		final a = new ClassA();
        final b = new ClassB();

        final fnA:Int->Void = a.doSomething;
        final fnB:Int->Void = b.doSomethingElse;

        f(fnA == fnB);
        f(Reflect.compareMethods(fnA, fnB));

        final castA:Dynamic->Void = cast fnA;
        final castB:Dynamic->Void = cast fnB;

        f(castA == castB);
        f(Reflect.compareMethods(castA, castB));

        var holder = new GenericHolder<Dynamic->Void>();
        holder.value = castA;

        f(holder.value == castB);
	}
}