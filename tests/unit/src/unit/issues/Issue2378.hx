package unit.issues;

private enum Kind
{
  //stub
  Message;
  //variable
  Flag;
  VarHash(key:{ name:String, t:CType }, value:{ name:String, t:CType }, ?valueIsArray:Bool);
  Var(t:CType);
  //function
  Function(args:Array<{name:String, opt:Bool, t:CType}>, ?varArgs:Null<CType>);
  SubDispatch;
}

private typedef CType = String;

class Issue2378 extends Test {
	function test() {
		var arg = { name: "test", command:"something", aliases:["a"], description:null, kind: Flag };
		var versions = arg.aliases != null ? arg.aliases.concat([arg.command]) : [arg.command];
		versions = versions.filter(function(s) return s != null && s != "");

		var prefix = "-";
		if (arg.kind == SubDispatch || arg.kind == Message)
			prefix = "";
		var a = versions.map(function(v) return (v.length == 1) ? prefix + v : prefix + prefix + v);
		eq(2, a.length);
		eq("-a", a[0]);
		eq("--something", a[1]);
	}
}