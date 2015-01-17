package unit.issues;

@:generic
private class BaseClass<T>
{
    public function new()
    {
        haxe.Log;
    }
}

private class SubClassOne extends BaseClass<Int>
{
    public function new() { super(); haxe.Log; }
}

private class SubClassTwo extends BaseClass<String>
{
    public function new() { super(); haxe.Log; }
}

class Issue3615 extends Test {
	function test() {

	}
}