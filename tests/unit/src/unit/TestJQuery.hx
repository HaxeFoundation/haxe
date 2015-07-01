package unit;

import js.jquery.*;
import js.jquery.Helper.*;

class TestJQuery extends Test {

    function testBasic() {
        var j = new JQuery("<div></div>");
        eq(j.length, 1);
    }

    function testRenamedStaticFields() {
        JQuery.eachStatic([123], function(i, e) {
            eq(i, 0);
            eq(e, 123);
        });
    }

    function testHelper() {
        var j = J("<div id='test'></div>");
        j.each(function(i,e){
            eq(JTHIS.attr("id"), "test");
        });
    }

}