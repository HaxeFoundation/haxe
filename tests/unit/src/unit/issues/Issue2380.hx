package unit.issues;

class Issue2380 extends unit.Test
{
	public function test()
	{
		var cls:Dynamic = new SomeCls();
		t(cls.test(null));
	}
}

#if !cpp @:nativeGen #end private class SomeCls
{
    public function new():Void {}
    @:keep public function test(v) return v == null;
}
