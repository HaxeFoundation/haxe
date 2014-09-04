package unit;
import sys.db.Object;
import sys.db.Types;

class MySpodClass extends Object
{
  public var theId:SId;
  public var int:SInt;
  public var double:SFloat;
  public var boolean:SBool;
  public var string:SString<255>;
  public var date:SDateTime;
  public var binary:SBinary;

  public var nullInt:SNull<Int>;
  public var enumFlags:SFlags<SpodEnum>;

  @:relation(rid) public var relation:OtherSpodClass;
  @:relation(rnid) public var relationNullable:Null<OtherSpodClass>;

  public var data:SData<Array<ComplexClass>>;
  public var anEnum:SEnum<SpodEnum>;
}

class ComplexClass
{
	public var val : { name:String, array:Array<String> };

	public function new(val)
	{
		this.val = val;
	}
}

class OtherSpodClass extends Object
{
	public var theid:SId;
	public var name:SString<255>;

	public function new(name:String)
	{
		super();
		this.name =name;
	}
}

enum SpodEnum
{
	FirstValue;
	SecondValue;
	ThirdValue;
}
