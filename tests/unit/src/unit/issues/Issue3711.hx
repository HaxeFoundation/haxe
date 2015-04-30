package unit.issues;

private abstract A<K>(haxe.DynamicAccess<Int>) {
    public function f():Array<K> return [];
}

class Issue3711 extends Test {
    public function test()
		{
			var a:A<String> = null;
			var k = a.f();
			eq(k[0],null);
			eq(k.length,0);
    }
}
