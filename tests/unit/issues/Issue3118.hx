package unit.issues;

class Issue3118 extends Test
{
	public function test()
	{
		var data = new Final();
		data.a2 = [];
		data.a2.push(1);
		eq(data.a2[0],1);
		eq(data.a2.length,1);
	}
}

class Base<A>
{
  public function new() {}

  public var a2:A;

}

@:final
class Final extends Base<Array<Int>>
{
}
