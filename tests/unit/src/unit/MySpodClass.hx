package unit;
import sys.db.Object;
import sys.db.Types;

@:keep class MySpodClass extends Object
{
	public var theId:SId;
	public var int:SInt;
	public var double:SFloat;
	public var boolean:SBool;
	public var string:SString<255>;
	public var date:SDateTime;
	public var binary:SBinary;
	public var abstractType:AbstractSpodTest<String>;

	public var nullInt:SNull<Int>;
	public var enumFlags:SFlags<SpodEnum>;

	@:relation(rid) public var relation:OtherSpodClass;
	@:relation(rnid) public var relationNullable:Null<OtherSpodClass>;
	@:relation(spid) public var next:Null<MySpodClass>;

	public var data:SData<Array<ComplexClass>>;
	public var anEnum:SEnum<SpodEnum>;
	public var bytes:SBytes<2>;
}

@:keep class NullableSpodClass extends Object
{
	public var theId:SId;
	@:relation(rnid) public var relationNullable:Null<OtherSpodClass>;
	public var data:Null<SData<Array<ComplexClass>>>;
	public var anEnum:Null<SEnum<SpodEnum>>;

	public var int:SNull<SInt>;
	public var double:SNull<SFloat>;
	public var boolean:SNull<SBool>;
	public var string:SNull<SString<255>>;
	public var date:SNull<SDateTime>;
	public var binary:SNull<SBinary>;
	public var abstractType:SNull<AbstractSpodTest<String>>;

	public var nullInt:SNull<Int>;
	public var enumFlags:SNull<SFlags<SpodEnum>>;
}

@:keep class ComplexClass
{
	public var val : { name:String, array:Array<String> };

	public function new(val)
	{
		this.val = val;
	}
}

@:id(theid) @:keep class OtherSpodClass extends Object
{
	public var theid:SInt;
	public var name:SString<255>;

	public function new(name:String)
	{
		super();
		this.name =name;
	}
}

@:keep enum SpodEnum
{
	FirstValue;
	SecondValue;
	ThirdValue;
}

abstract AbstractSpodTest<A>(A) from A
{
	public function get():A
	{
		return this;
	}
}

@:id(name)
	@:keep class ClassWithStringId extends Object
{
	public var name:SString<255>;
	public var field:SInt;
}

@:keep class ClassWithStringIdRef extends Object
{
	public var id:SId;
	@:relation(ref_id) public var ref:ClassWithStringId;
}


//issue #3828
@:keep @:skip class BaseIssueC3828 extends sys.db.Object {
	public var id : SInt;
	@:relation(ruid)
		public var refUser : SNull<IssueC3828>;
}

@:keep class IssueC3828 extends BaseIssueC3828 {
}

@:keep class Issue6041Table extends Object {
	public var id:SInt = 0;
}