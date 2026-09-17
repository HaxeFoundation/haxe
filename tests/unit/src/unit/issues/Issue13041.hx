package unit.issues;

class Issue13041 extends Test {
	function errorTryCatch( pos = 0, length = 100 ) {
		try {
			while( true ) {
				pos += 10;
				length -= 10;
				if( length <= 40 )
					throw haxe.io.Error.Blocked;
			}
		} catch( e : haxe.io.Error ) {
			eq(pos, 60);
			eq(length, 40);
		}
		eq(pos, 60);
		eq(length, 40);
	}

	function test() {
		errorTryCatch();
	}
}
