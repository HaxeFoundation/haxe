package unit.issues;

class Issue9372 extends unit.Test {
  function test() {
    var data = {foo: '!', bar: [0, 1]};
    var template = new haxe.Template("::foreach bar:: ::foo:: ::__current__:: ::end::");
    var output = template.execute(data);
    eq(" ! 0  ! 1 ", output);
  }
}
