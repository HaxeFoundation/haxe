package unit.issues;

class Issue2042 extends Test
{
	public function test()
	{
		var m = haxe.rtti.Meta.getType(WithMeta);
		t(m.someMeta != null);
		eq(m.someMeta[0],1);
		t(m.otherMeta != null);
		eq(m.otherMeta[0],2);
		t(haxe.rtti.Meta.getFields(WithMeta).testing != null);
		eq(haxe.rtti.Meta.getFields(WithMeta).testing.varMeta[0], 3);
	}
}

@someMeta(1) @otherMeta(2)
@:keep private interface WithMeta
{
	@varMeta(3)
	function testing():Void;
}
