package unit;

class TestMisc extends Test {

	public function testString() {
		eq( String.fromCharCode(77), "M" );
		unspec(function() String.fromCharCode(0));
		unspec(function() String.fromCharCode(-1));
		unspec(function() String.fromCharCode(256));
	}

	public function testClosure() {
		var c = new MyClass(100);
		var add = c.add;
		eq( c.add(1,2), 103 );
		eq( callback(c.add,1)(2), 103 );
		eq( add(1,2), 103 );

		var x = 4;
		var f = function() return x;
		eq( f(), 4 );
		x++;
		eq( f(), 5 );
	}

	public function testBlockVars() {
		var a = new Array();
		for( i in 0...10 )
			a.push(function() return i);
		for( i in 0...10 )
			eq( a[i](), i );
	}

	public function testScopeVar() {
		var x = 4;
		{
			var x = "hello";
			eq(x,"hello");
			switch( MyEnum.C(66,"") ) {
			case C(x,_):
				eq(x,66);
			default:
			}
		}
		eq(x,4);
	}

}