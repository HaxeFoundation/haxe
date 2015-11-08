package unit.issues;

class Issue4238 extends Test {

	public function new() 
	{
		super();
	}
	
	public function test() {
		var a : Array<Null<Int>> = null;
		
		a = [5, 6];
		var res = a.removeAt(0);
		eq( 5, res );
		eq(1, a.length);
		
		a = [5, 6];
		var res = a.removeAt(1);
		eq(6, res );
		eq(1, a.length);
		
		a = [5, 6];
		eq( 6, a.removeAt(1));
		a = [5, 6];
		eq( 6, a.removeAt( -1));
		a = [5, 6];
		eq( 5, a.removeAt(0));
		
		a = [5, 6];
		eq( 6, a.removeAt( -1));
		
		a = [5, 6];
		eq( 5, a.removeAt( -2));
		
		a = [5];
		a.remove(5);
		eq( null, a[0] );
		
		a = [5];
		var e = a.removeAt( -1);
		eq(0, a.length);
		eq(5, e);
		
		a = [];
		var e = a.removeAt( -1);
		eq(null, e);
		
		a = [5,6];
		var e = a.removeAt( 4);
		eq(null, e);
	}
}