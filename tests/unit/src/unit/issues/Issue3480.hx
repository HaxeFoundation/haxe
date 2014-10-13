package unit.issues;

class Issue3480 extends Test {
    function test() {
        #if js
        eq("{\n\ttoString : 1\n}", Std.string({toString: 1}));
        #end
    }
}
