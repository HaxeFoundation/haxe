package unit.issues;

class Issue6457 extends Test {
  public function test() {
    var m = new Map();
    m.set(24, 124);
    m.set(0, 11);
    m.remove(24);
    m.set(0, 10);
    var keys = [ for (k in m.keys()) k];
    eq(keys.length, 1);
    var vals = [ for (k in m) k];
    eq(vals.length, 1);
    eq(vals[0], 10);
    eq(m.toString(), '[0 => 10]');

    var m = new Map();
    m.set("c", 1);
    m.set("z", 1);
    var keys = [ for (k in m.keys()) k];
    m.remove("c");
    m.set("z", 10);
    var keys = [ for (k in m.keys()) k];
    eq(keys.length, 1);
    var vals = [ for (k in m) k];
    eq(vals.length, 1);
    eq(vals[0], 10);
    eq(m.toString(), '[z => 10]');
  }
}
