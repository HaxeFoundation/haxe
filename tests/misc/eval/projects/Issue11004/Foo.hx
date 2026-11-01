class Foo
{
  var _bar_private:Bar;

  public function new()
  {
    trace('New foo');
    _bar_private = new Bar();
  }

  public var bar(get,null):Bar;
  public function get_bar():Bar return _bar_private;
}
