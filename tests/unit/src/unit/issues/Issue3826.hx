package unit.issues;

class Issue3826 extends Test {

	var inull : Null<Int> = null;
	var fnull : Null<Float> = null;

	function get( a = 2, b = 4.25 ) {
		return ""+a+"/"+b;
	}

	public function test() {
		eq( get(), "2/4.25" );
		eq( get(5), "5/4.25" );
		eq( get(5,8.5), "5/8.5" );
		
		#if !flash
		eq( get(8.5), "2/8.5" );
		#end

		#if static
		eq( get(inull), "0/4.25" );
		eq( get(inull,fnull), "0/0" );

		#if !flash
		eq( get(fnull), "2/0" );
		#end
		
		#else
		eq( get(inull), "2/4.25" );
		eq( get(inull,fnull), "2/4.25" );
		
		#if !flash
		eq( get(fnull), "2/4.25" );
		#end
		
		#end

	
	}

	function getOpt( ?a = 2, ?b = 4.25 ) {
		return ""+a+"/"+b;
	}

	public function testOpt() {
		eq( getOpt(), "2/4.25" );
		eq( getOpt(5), "5/4.25" );
		eq( getOpt(5,8.5), "5/8.5" );
		eq( getOpt(8.5), "2/8.5" );

		eq( getOpt(inull), "2/4.25");
		eq( getOpt(inull,fnull), "2/4.25");
		eq( getOpt(fnull), "2/4.25");
	}

}
