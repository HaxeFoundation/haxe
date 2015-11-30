package unit;

import js.jquery.*;
import js.jquery.Helper.*;

class TestJQuery extends Test {

	function testBasic() {
		var j = new JQuery("<div></div>");
		eq(j.length, 1);
	}

	function testRenamedStaticFields() {
		JQuery.each([123], function(i, e) {
			eq(i, 0);
			eq(e, 123);
		});
	}

	function testIterator() {
		var div = Lambda.find(new JQuery("<div></div>"), function(ele) return ele.tagName.toLowerCase() == "div");
		eq(div.tagName.toLowerCase(), "div");
	}

	function testHelper() {
		var j = J("<div id='test'></div>");
		j.each(function(i,e){
			eq(JTHIS.attr("id"), "test");
		});
	}

	function testVersion() {
		var versionStr:String = JQuery.fn.jquery;
		var v = versionStr.split(".");
		eq(v.length, 3);
		eq(haxe.macro.Compiler.getDefine("jquery_ver"), v[0] + StringTools.lpad(v[1], "0", 2) + StringTools.lpad(v[2], "0", 2));
	}

}