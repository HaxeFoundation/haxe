package unit.issues;

class Issue8560 extends unit.Test {
	function test() {
		var ctx	= {
			name : "Foo",
			isMale : false,
			len : 5
		}
		//Spaces are important in this string. Do not remove.
		var s = '::if  (  (  name  ==  "Foo"  )  &&  (  isMale  )  )  ::Ok::elseif isMale ::Maybe::else::No::end::';
		var t = new haxe.Template(s);
		eq('No', t.execute(ctx));
	}
}