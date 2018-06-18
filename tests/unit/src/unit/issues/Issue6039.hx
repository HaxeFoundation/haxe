package unit.issues;
#if cs
import cs.system.Action_1;
#end

class Issue6039 extends Test
{
  public function test()
  {
#if cs
    var myStr = null;
    var c:Callback = function(str) myStr = str;
    c.invokeIt('test');
    this.eq(myStr, 'test');
#end
  }
}

#if cs
@:keep
private abstract Callback(cs.system.Action_1<String>) from Action_1<String> to Action_1<String>{
	function new(f)
		this = f;

	public function invokeIt(data:String):Void
		this.Invoke(data);

	@:from static function fromHaxeFunc(foo:String->Void):Callback
		return new Callback(new Action_1<String>(foo));
}
#end