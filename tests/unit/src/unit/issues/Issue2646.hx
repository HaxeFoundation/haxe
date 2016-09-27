package unit.issues;

class Issue2646 extends unit.Test
{
	public function test()
	{
		var b:{?a:Bool} = {};
		t(b.a == null);
		f(b.a != null);
		var c = { b: b };
		t(c.b.a == null);
		f(c.b.a != null);
		var d = { c : c };
		t(d.c.b.a == null);
		f(d.c.b.a != null);
		
		var a = {};
		var b:{?a:Bool} = a;
		t(b.a == null);
		f(b.a != null);
		var c = { b: b };
		t(c.b.a == null);
		f(c.b.a != null);
		var d = { c : c };
		t(d.c.b.a == null);
		f(d.c.b.a != null);
	}
}
