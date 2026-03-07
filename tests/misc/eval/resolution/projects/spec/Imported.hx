import RootModWithStatic;
import pack.ModWithStatic;
import utest.Assert;

class Imported extends utest.Test {
    function test() {
        Assert.equals("pack.ModWithStatic.TheStatic function", ModWithStatic.TheStatic());
        Assert.equals("RootModWithStatic.TheStatic function", RootModWithStatic.TheStatic());
        Assert.equals("pack.TheStatic", Type.getClassName(TheStatic));
    }
}
