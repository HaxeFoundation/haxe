package unit.issues;

class Issue2645 extends unit.Test
{
	public function test()
	{
		var a:{b:Maybe<Bool>} = {b:null};
		f(a.b.or(false));
		t(a.b.or(true));
	}
}

private abstract Maybe<T>(Null<T>) from T
{
	public inline function or(defaultValue:T):T return this != null ? this : defaultValue;
}