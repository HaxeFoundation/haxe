package unit.issues;

class Issue5747 extends Test {
    #if cs
    function test() {
        var a = [1,2,3];
        eq(@:privateAccess a.__a[0], 1);
    }
    #end
}
