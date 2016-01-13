package unit.issues;

class Issue4238 extends Test {

	public function new() 
	{
		super();
	}
	
	public function test() {
		#if (js || flash || cpp || neko)
		
		var a : Array<Null<Int>> = null;
		
		a = [5, 6];
		var res : Null<Int> = a[0];
		var b  : Bool = a.removeAt(0);
		eq( true, b );
		eq( 5, res );
		eq( 1, a.length);
		
		a = [];
		var b  : Bool = a.removeAt(0);
		eq( false, b );
		
		a = [5, 6];
		var res = a[1];
		a.removeAt(1);
		eq(6, res );
		eq(1, a.length);
		
		a = [5, 6];
		var res = a[1];
		a.removeAt(1);
		eq( 6, res);
		a = [5, 6];
		var res = a[a.length - 1];
		a.removeAt( -1 );
		eq( 6, res);
		a = [5, 6];
		var res = a[0];
		a.removeAt(0);
		eq( 5, res);
		
		a = [5, 6];
		var res = a[a.length - 1];
		a.removeAt(-1);
		eq( 6, res);
		
		a = [5, 6];
		var res = a[a.length - 2];
		a.removeAt(-2);
		eq( 5, res );
		
		#else
		eq(0,0);
		#end
	}
}