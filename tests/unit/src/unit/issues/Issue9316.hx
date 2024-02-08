package unit.issues;

class Issue9316 extends unit.Test {
	var opt:Options = {};

	function test() {
		var fn = switch opt {
			case {fn:null}: () -> 'ok';
			case _: () -> 'fail';
		}
		eq('ok', fn());
	}
}

private typedef Options = {
	final ?fn:()->String;
}