package unit.issues;

class Issue4918 extends Test {
	static var p:{ ?opt:Bool } = { opt: true };
	function test() {
		var hasOpt = false, minParams = 0;
		if( p.opt )
			hasOpt = true;
		else
			minParams++;
		var f = function() {
			trace(minParams);
		};
	}
}