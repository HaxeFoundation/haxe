package unit.issues;

class Issue8861 extends Test {
	function test() {
		var str = ~/[äöü]/gu.map('väter, söhne, mütter', rgx -> switch rgx.matched(0) {
			case 'ä' : 'ae'; case 'ö' : 'oe';
			case 'ü' : 'ue'; case _ : '';
		});
		eq('vaeter, soehne, muetter', str);
	}
}