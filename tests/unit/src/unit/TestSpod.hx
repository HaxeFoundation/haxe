package unit;
import sys.db.*;
import haxe.io.Bytes;
import haxe.EnumFlags;
import sys.db.Connection;
import sys.db.Manager;
import sys.db.Sqlite;
import sys.db.TableCreate;
import sys.FileSystem;
import unit.MySpodClass;

using Lambda;

class TestSpod extends Test
{
	private var cnx:Connection;
	public function new(cnx:Connection)
	{
		super();
		this.cnx = cnx;
		Manager.cnx = cnx;
		try cnx.request('DROP TABLE MySpodClass') catch(e:Dynamic) {}
		try cnx.request('DROP TABLE OtherSpodClass') catch(e:Dynamic) {}
		try cnx.request('DROP TABLE NullableSpodClass') catch(e:Dynamic) {}
		try cnx.request('DROP TABLE ClassWithStringId') catch(e:Dynamic) {}
		try cnx.request('DROP TABLE ClassWithStringIdRef') catch(e:Dynamic) {}
		try cnx.request('DROP TABLE IssueC3828') catch(e:Dynamic) {}
		TableCreate.create(MySpodClass.manager);
		TableCreate.create(OtherSpodClass.manager);
		TableCreate.create(NullableSpodClass.manager);
		TableCreate.create(ClassWithStringId.manager);
		TableCreate.create(ClassWithStringIdRef.manager);
		TableCreate.create(IssueC3828.manager);
	}

	private function setManager()
	{
		Manager.initialize();
		Manager.cnx = cnx;
		Manager.cleanup();
	}

	function getDefaultClass()
	{
		var scls = new MySpodClass();
		scls.int = 1;
		scls.double = 2.0;
		scls.boolean = true;
		scls.string = "some string";
		scls.date = new Date(2012, 7, 30, 0, 0, 0);
		scls.abstractType = "other string";

		var bytes = Bytes.ofString("\x01\n\r'\x02");
		scls.binary = bytes;
		scls.enumFlags = EnumFlags.ofInt(0);
		scls.enumFlags.set(FirstValue);
		scls.enumFlags.set(ThirdValue);

		scls.data = [new ComplexClass( { name:"test", array:["this", "is", "a", "test"] } )];
		scls.anEnum = SecondValue;

		return scls;
	}

	public function testIssue3828()
	{
		setManager();
		var u1 = new IssueC3828();
		u1.insert();
		var u2 = new IssueC3828();
		u2.refUser = u1;
		u2.insert();
		var u1id = u1.id, u2id = u2.id;
		u1 = null; u2 = null;
		Manager.cleanup();

		var u1 = IssueC3828.manager.get(u1id);
		var u2 = IssueC3828.manager.search($refUser == u1).first();
		eq(u1.id, u1id);
		eq(u2.id, u2id);
	}

	public function testStringIdRel()
	{
		setManager();
		var s = new ClassWithStringId();
		s.name = "first";
		s.field = 1;
		s.insert();
		var v1 = new ClassWithStringIdRef();
		v1.ref = s;
		v1.insert();
		var v2 = new ClassWithStringIdRef();
		v2.ref = s;
		v2.insert();

		s = new ClassWithStringId();
		s.name = "second";
		s.field = 2;
		s.insert();
		v1 = new ClassWithStringIdRef();
		v1.ref = s;
		v1.insert();
		s = null; v1 = null; v2 = null;
		Manager.cleanup();

		var first = ClassWithStringId.manager.search($name == "first");
		eq(first.length,1);
		var first = first.first();
		eq(first.field,1);
		var frel = ClassWithStringIdRef.manager.search($ref == first);
		eq(frel.length,2);
		for (rel in frel)
			eq(rel.ref, first);
		var frel2 = ClassWithStringIdRef.manager.search($ref_id == "first");
		eq(frel2.length,2);
		for (rel in frel2)
			eq(rel.ref, first);

		var second = ClassWithStringId.manager.search($name == "second");
		eq(second.length,1);
		var second = second.first();
		eq(second.field,2);
		var srel = ClassWithStringIdRef.manager.search($ref == second);
		eq(srel.length,1);
		for (rel in srel)
			eq(rel.ref, second);

		eq(frel.array().indexOf(srel.first()), -1);
		second.delete();
		for (r in srel) r.delete();
		first.delete();
		for (r in frel) r.delete();
	}

	public function testEnum()
	{
		setManager();
		var c1 = new OtherSpodClass("first spod");
		c1.insert();
		var c2 = new OtherSpodClass("second spod");
		c2.insert();

		var scls = getDefaultClass();
		var scls1 = scls;
		scls.relation = c1;
		scls.insert();
		var id1 = scls.theId;
		scls = getDefaultClass();
		scls.relation = c1;
		scls.insert();

		scls1.next = scls;
		scls1.update();

		var id2 = scls.theId;
		scls = getDefaultClass();
		scls.relation = c1;
		scls.next = scls1;
		scls.anEnum = FirstValue;
		scls.insert();
		var id3 = scls.theId;
		scls = null;

		Manager.cleanup();
		var r1s = [ for (c in MySpodClass.manager.search($anEnum == SecondValue,{orderBy:theId})) c.theId ];
		eq([id1,id2].join(','),r1s.join(','));
		var r2s = MySpodClass.manager.search($anEnum == FirstValue);
		eq(r2s.length,1);
		eq(r2s.first().theId,id3);
		eq(r2s.first().next.theId,id1);
		eq(r2s.first().next.next.theId,id2);

		var fv = getSecond();
		var r1s = [ for (c in MySpodClass.manager.search($anEnum == fv,{orderBy:theId})) c.theId ];
		eq([id1,id2].join(','),r1s.join(','));
		var r2s = MySpodClass.manager.search($anEnum == getFirst());
		eq(r2s.length,1);
		eq(r2s.first().theId,id3);

		var ids = [id1,id2,id3];
		var s = [ for (c in MySpodClass.manager.search( $anEnum == SecondValue || $theId in ids )) c.theId ];
		s.sort(Reflect.compare);
		eq([id1,id2,id3].join(','),s.join(','));

		r2s.first().delete();
		for (v in MySpodClass.manager.search($anEnum == fv)) v.delete();
	}

	public function getFirst()
	{
		return FirstValue;
	}

	public function getSecond()
	{
		return SecondValue;
	}

	public function testUpdate()
	{
		setManager();
		var c1 = new OtherSpodClass("first spod");
		c1.insert();
		var c2 = new OtherSpodClass("second spod");
		c2.insert();
		var scls = getDefaultClass();
		scls.relation = c1;
		scls.relationNullable = c2;
		scls.insert();

		var id = scls.theId;

		//if no change made, update should return nothing
		eq( untyped MySpodClass.manager.getUpdateStatement( scls ), null );
		Manager.cleanup();
		scls = MySpodClass.manager.get(id);
		eq( untyped MySpodClass.manager.getUpdateStatement( scls ), null );
		scls.delete();

		//try now with null SData and null relation
		var scls = new NullableSpodClass();
		scls.insert();

		var id = scls.theId;

		//if no change made, update should return nothing
		eq( untyped NullableSpodClass.manager.getUpdateStatement( scls ), null );
		Manager.cleanup();
		scls = NullableSpodClass.manager.get(id);
		eq( untyped NullableSpodClass.manager.getUpdateStatement( scls ), null );
		eq(scls.data,null);
		eq(scls.relationNullable,null);
		eq(scls.abstractType,null);
		eq(scls.anEnum,null);
		scls.delete();

		//same thing with explicit null set
		var scls = new NullableSpodClass();
		scls.data = null;
		scls.relationNullable = null;
		scls.abstractType = null;
		scls.anEnum = null;
		scls.insert();

		var id = scls.theId;

		//if no change made, update should return nothing
		eq( untyped NullableSpodClass.manager.getUpdateStatement( scls ), null );
		Manager.cleanup();
		scls = NullableSpodClass.manager.get(id);
		eq( untyped NullableSpodClass.manager.getUpdateStatement( scls ), null );
		eq(scls.data,null);
		eq(scls.relationNullable,null);
		eq(scls.abstractType,null);
		eq(scls.anEnum,null);
		Manager.cleanup();

		scls = new NullableSpodClass();
		scls.theId = id;
		t( untyped NullableSpodClass.manager.getUpdateStatement( scls ) != null );

		scls.delete();
	}

	public function testSpodTypes()
	{
		setManager();
		var c1 = new OtherSpodClass("first spod");
		c1.insert();
		var c2 = new OtherSpodClass("second spod");
		c2.insert();

		var scls = getDefaultClass();

		scls.relation = c1;
		scls.relationNullable = c2;
		scls.insert();

		//after inserting, id must be filled
		t(scls.theId != 0 && scls.theId != null,pos());
		var theid = scls.theId;

		c1 = c2 = null;
		Manager.cleanup();

		var cls1 = MySpodClass.manager.get(theid);
		t(cls1 != null,pos());
		//after Manager.cleanup(), the instances should be different
		f(cls1 == scls,pos());
		scls = null;

		t((cls1.int is Int),pos());
		eq(cls1.int, 1,pos());
		t((cls1.double is Float),pos());
		eq(cls1.double, 2.0,pos());
		t((cls1.boolean is Bool),pos());
		eq(cls1.boolean, true,pos());
		t((cls1.string is String),pos());
		eq(cls1.string, "some string",pos());
		t((cls1.abstractType is String),pos());
		eq(cls1.abstractType.get(), "other string",pos());
		t(cls1.date != null,pos());
		t((cls1.date is Date),pos());
		eq(cls1.date.getTime(), new Date(2012, 7, 30, 0, 0, 0).getTime(),pos());

		t((cls1.binary is Bytes),pos());
		eq(cls1.binary.compare(Bytes.ofString("\x01\n\r'\x02")), 0,pos());
		t(cls1.enumFlags.has(FirstValue),pos());
		f(cls1.enumFlags.has(SecondValue),pos());
		t(cls1.enumFlags.has(ThirdValue),pos());

		t((cls1.data is Array),pos());
		t((cls1.data[0] is ComplexClass),pos());

		eq(cls1.data[0].val.name, "test",pos());
		eq(cls1.data[0].val.array.length, 4,pos());
		eq(cls1.data[0].val.array[1], "is",pos());

		eq(cls1.relation.name, "first spod",pos());
		eq(cls1.relationNullable.name, "second spod",pos());

		eq(cls1.anEnum, SecondValue,pos());
		t((cls1.anEnum is SpodEnum),pos());

		eq(cls1, MySpodClass.manager.select($anEnum == SecondValue),pos());

		//test create a new class
		var scls = getDefaultClass();

		c1 = new OtherSpodClass("third spod");
		c1.insert();

		scls.relation = c1;
		scls.insert();

		scls = cls1 = null;
		Manager.cleanup();

		eq(2, MySpodClass.manager.all().length,pos());
		var req = MySpodClass.manager.search({ relation: OtherSpodClass.manager.select({ name:"third spod"} ) });
		eq(req.length, 1,pos());
		scls = req.first();

		scls.relation.name = "Test";
		scls.relation.update();

		eq(OtherSpodClass.manager.select({ name:"third spod" }), null,pos());

		for (c in MySpodClass.manager.all())
			c.delete();
		for (c in OtherSpodClass.manager.all())
			c.delete();

		//issue #3598
		var inexistent = MySpodClass.manager.get(1000,false);
		eq(inexistent,null);
	}

	public function testDateQuery()
	{
		setManager();
		var other1 = new OtherSpodClass("required field");
		other1.insert();

		var now = Date.now();
		var c1 = getDefaultClass();
		c1.relation = other1;
		c1.date = now;
		c1.insert();

		var c2 = getDefaultClass();
		c2.relation = other1;
		c2.date = DateTools.delta(now, DateTools.hours(1));
		c2.insert();

		var q = MySpodClass.manager.search($date > now);
		eq(q.length, 1,pos());
		eq(q.first(), c2,pos());

		q = MySpodClass.manager.search($date == now);
		eq(q.length, 1,pos());
		eq(q.first(), c1,pos());

		q = MySpodClass.manager.search($date >= now);
		eq(q.length, 2,pos());
		eq(q.first(), c1,pos());

		q = MySpodClass.manager.search($date >= DateTools.delta(now, DateTools.hours(2)));
		eq(q.length, 0,pos());
		eq(q.first(), null,pos());

		c1.delete();
		c2.delete();
		other1.delete();
	}

	public function testData()
	{
		setManager();
		var other1 = new OtherSpodClass("required field");
		other1.insert();

		var c1 = getDefaultClass();
		c1.relation = other1;
		c1.insert();

		eq(c1.data.length,1,pos());
		c1.data.pop();
		c1.update();

		Manager.cleanup();
		c1 = null;

		c1 = MySpodClass.manager.select($relation == other1);
		eq(c1.data.length, 0,pos());
		c1.data.push(new ComplexClass({ name: "test1", array:["complex","field"] }));
		c1.data.push(null);
		eq(c1.data.length, 2,pos());
		c1.update();

		Manager.cleanup();
		c1 = null;

		c1 = MySpodClass.manager.select($relation == other1);
		eq(c1.data.length,2,pos());
		eq(c1.data[0].val.name, "test1",pos());
		eq(c1.data[0].val.array.length, 2,pos());
		eq(c1.data[0].val.array[0], "complex",pos());
		eq(c1.data[1], null,pos());

		c1.delete();
		other1.delete();
	}

	private function pos(?p:haxe.PosInfos):haxe.PosInfos
	{
		p.fileName = p.fileName + "(" + cnx.dbName()  +")";
		return p;
	}
}
