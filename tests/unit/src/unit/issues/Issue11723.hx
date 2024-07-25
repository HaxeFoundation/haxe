package unit.issues;

@:keep
private interface IEntity {
	var id:Int;
	function getVariable(varName:String):IVariable;
	function containsVariable(varName:String):Bool;
}

@:keep
private interface IUser extends IEntity {
	var id:Int;
	var name:String;
	function getVariable(name:String):IUserVariable;
	function setVariable(userVariable:IUserVariable):Void;
}

private class BasicUser implements IUser {
	public var id:Int;
	public var name:String;

	private var _variables:haxe.ds.StringMap<IUserVariable>;

	public function new(id:Int, name:String) {
		this.id = id;
		this.name = name;
		_variables = new haxe.ds.StringMap();
	}

	public function getVariable(name:String):IUserVariable {
		var get = _variables.get(name);
		return cast(get, IUserVariable);
	}

	public function containsVariable(name:String):Bool {
		var get = _variables.get(name);
		return get != null;
	}

	public function setVariable(userVariable:IUserVariable):Void {
		if (userVariable != null) {
			_variables.set(userVariable.name, userVariable);
		}
	}
}

@:keep
private interface IVariable {
	var name:String;
	var type:String;
	function getValue():Dynamic;
	function getIntValue():Int;
	function getStringValue():String;
}

@:keep
private interface IUserVariable extends IVariable {
	var isPrivate:Bool;
}

private class BaseVariable implements IVariable {
	public var name:String;
	public var type:String;

	private var _value:Any;

	public function new(name:String, value:Any, type:String) {
		this.name = name;
		this.type = type;
		_value = value;
	}

	public function getValue():Dynamic {
		return _value;
	}

	public function getIntValue():Int {
		return cast(_value, Int);
	}

	public function getStringValue():String {
		return cast(_value, String);
	}
}

private class BasicUserVariable extends BaseVariable implements IUserVariable {
	public var isPrivate:Bool;

	public function new(name:String, value:Any, type:String) {
		super(name, value, type);
	}
}

class Issue11723 extends Test {
	public function test() {
		var varname = "TestVar";
		var myVariable = new BasicUserVariable(varname, "Testvalue", "String");
		eq("Testvalue", myVariable.getStringValue());

		var myUser = new BasicUser(1, "myUser");
		myUser.setVariable(myVariable);
		var formUser = myUser.getVariable(varname);
		eq("Testvalue", formUser.getStringValue());

		var bareEntity:IEntity = myUser;
		var fromBareEntity = bareEntity.getVariable(varname);
		eq("Testvalue", fromBareEntity.getStringValue());
		t(bareEntity.containsVariable(varname));

		var bareUser:IUser = myUser;
		var fromBareUser = bareUser.getVariable(varname);
		eq("Testvalue", fromBareUser.getStringValue());
		t(bareUser.containsVariable(varname));
	}
}
