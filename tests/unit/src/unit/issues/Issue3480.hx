package unit.issues;

class Issue3480 extends Test {
    function test() {
        #if js
        if (!js.Browser.supported || js.Browser.navigator.userAgent.indexOf('MSIE 8') == -1) // IE8 doesn't like toString fields at all
            eq("{\n\ttoString : 1\n}", Std.string({toString: 1}));
        #end
    }
}
