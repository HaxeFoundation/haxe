package unit.issues;

class Issue4085 extends Test {
    #if js
    function test() {
        function throwError() throw "hello, world";
        var msg = null;
        untyped __js__("try { throwError(); } catch (e) { msg = e.message; }");
        eq(msg, "hello, world");
    }
    #end
}
