package unit.issues;

class Issue2281 extends Test
{
    function test() {
			var fint = new Foo();
			fint.val = 0;
			eq(0, fint.val++);
			eq(1,fint.val);
			eq(2,++fint.val);
			eq(12, fint.val += 10);
			fint.valNull = 0;
			eq(0, fint.valNull++);
			eq(1,fint.valNull);
			eq(2,++fint.valNull);
			eq(12, fint.valNull += 10);
			fint.val2 = 0;
			eq(0, fint.val2++);
			eq(1,fint.val2);
			eq(2,++fint.val2);
			eq(12, fint.val2 += 10);
    }
}


private class Foo<T>
{
	public var val:T;
	public var valNull:Null<T>;
	public var val2:Null<Int>;
	public function new()
	{
	}
}
