package unit.issues;

class Issue6291 extends Test
{
  public function test()
  {
    var b = new BadCode();
    eq(b.d1.get('a'), 1);
  }
}

#if java
@:nativeGen @:keep
#end
private class BadCode {
  public var d1:Dictionary= new Dictionary();
  public var d2:Dictionary= new Dictionary();
  public var d3:Dictionary= new Dictionary();
  public var d4:Dictionary= new Dictionary();
  public var d5:Dictionary;

  public function new() {
    d5 = new Dictionary();
  }
}

@:forward
private abstract Dictionary(Map<String, Dynamic>)
{
  inline public function new()
  {
    this = new Map();
    this.set('a', 1);
  }
}
