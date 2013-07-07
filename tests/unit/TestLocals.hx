package unit;

class TestLocals extends Test {

	function testIncrDecr() {
		var i = 5;
		eq( i++, 5 );
		eq( i, 6 );
		eq( i--, 6 );
		eq( i, 5 );
		eq( ++i, 6 );
		eq( i, 6 );
		eq( --i, 5 );
		eq( i, 5 );
	}

	function testScope() {
		var x = 0;
		eq(x,0);
		// simple scope
		{
			var x = "hello";
			eq(x,"hello");
			{
				var x = "";
				eq(x,"");
			}
			eq(x,"hello");
		}
		eq(x,0);
		// if
		var flag = true;
		if( flag ) {
			var x = "hello";
			eq(x,"hello");
		}
		eq(x,0);
		// for
		for( x in ["hello"] )
			eq(x,"hello");
		eq(x,0);
		// switch
		switch( MyEnum.D(MyEnum.A) ) {
		case D(x):
			eq(x,MyEnum.A);
		default:
			assert();
		}
		eq(x,0);
		// try/catch
		try {
			throw "hello";
		} catch( x : Dynamic ) {
			eq(x,"hello");
		}
		eq(x,0);
	}

	function testCapture() {
		// read
		var funs = new Array();
		for( i in 0...5 )
			funs.push(function() return i);
		for( k in 0...5 )
			eq(funs[k](),k);

		// write
		funs = new Array();
		var sum = 0;
		for( i in 0...5 ) {
			var k = 0;
			funs.push(function() { k++; sum++; return k; });
		}
		for( i in 0...5 )
			eq(funs[i](),1);
		eq(sum,5);

		// multiple
		var accesses = new Array();
		var sum = 0;
		for( i in 0...5 ) {
			var j = i;
			accesses.push({
				inc : function() { sum += j; j++; return j; },
				dec : function() { j--; sum -= j; return j; },
			});
		}
		for( i in 0...5 ) {
			var a = accesses[i];
			eq( a.inc(), i + 1 );
			eq( sum, i );
			eq( a.dec(), i );
			eq( sum, 0 );
		}
	}

	function testSubCapture() {
		var funs = new Array();
		for( i in 0...5 )
			funs.push(function() {
				var tmp = new Array();
				for( j in 0...5 )
					tmp.push(function() return i + j);
				var sum = 0;
				for( j in 0...5 )
					sum += tmp[j]();
				return sum;
			});
		for( i in 0...5 )
			eq( funs[i](), i * 5 + 10 );
	}

	function testParallelCapture() {
		var funs = new Array();
		for( i in 0...5 ) {
			if( true ) {
				var j = i;
				funs.push(function(k) return j);
			}
			if( true )
				funs.push(function(j) return j);
		}
		for( k in 0...5 ) {
			eq( funs[k*2](0), k );
			eq( funs[k*2+1](k), k );
		}
	}

	function testPossibleBug() {
		var funs = new Array();
		for( i in 0...5 )
			funs.push(function(i) return i);
		for( k in 0...5 )
			eq( funs[k](55), 55 );
	}

}