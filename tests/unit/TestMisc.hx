package unit;

class TestMisc extends Test {

	function testString() {
		eq( String.fromCharCode(77), "M" );
		unspec(function() String.fromCharCode(0));
		unspec(function() String.fromCharCode(-1));
		unspec(function() String.fromCharCode(256));
	}

	function testClosure() {
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

	function testBlockVars() {
		var a = new Array();
		for( i in 0...10 )
			a.push(function() return i);
		for( i in 0...10 )
			eq( a[i](), i );
	}

	function testScopeVar() {
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

	function testMD5() {
		eq( haxe.Md5.encode(""), "d41d8cd98f00b204e9800998ecf8427e" );
		eq( haxe.Md5.encode("hello"), "5d41402abc4b2a76b9719d911017c592" );
		// depending of ISO/UTF8 native
		allow( haxe.Md5.encode("héllo"), ["1a722f7e6c801d9e470a10cb91ba406d","be50e8478cf24ff3595bc7307fb91b50"] );
	}

}
