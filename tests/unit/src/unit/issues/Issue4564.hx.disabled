package unit.issues;

class Issue4564 extends Test {
	function test() {
		inline function test(str:String, ?pos:haxe.PosInfos) {
			eq(StringTools.urlDecode(StringTools.urlEncode(str)), str, pos);
		}
		eq(StringTools.urlEncode('me@home.com'), 'me%40home.com');
		test('me@home.com');
		eq(StringTools.urlEncode('Email Address'), 'Email%20Address');
		test('Email Address');
		eq(StringTools.urlEncode('Email Address!'), 'Email%20Address!');
		test('Email Address!');
		eq(StringTools.urlEncode('Email Address('), 'Email%20Address(');
		test('Email Address(');
		test('ção');
		test('a%a');
		test('a%20');
		test('a%20a');
		test('a%20a%');
	}
}
