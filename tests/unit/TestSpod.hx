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
		TableCreate.create(MySpodClass.manager);
		TableCreate.create(OtherSpodClass.manager);
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

		var bytes = Bytes.ofString("\x01\n\r\x02");
		scls.binary = bytes;
		scls.enumFlags = EnumFlags.ofInt(0);
		scls.enumFlags.set(FirstValue);
		scls.enumFlags.set(ThirdValue);

		scls.data = [new ComplexClass( { name:"test", array:["this", "is", "a", "test"] } )];
		scls.anEnum = SecondValue;

		return scls;
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

		t(Std.is(cls1.int, Int),pos());
		eq(cls1.int, 1,pos());
		t(Std.is(cls1.double, Float),pos());
		eq(cls1.double, 2.0,pos());
		t(Std.is(cls1.boolean, Bool),pos());
		eq(cls1.boolean, true,pos());
		t(Std.is(cls1.string, String),pos());
		eq(cls1.string, "some string",pos());
		t(cls1.date != null,pos());
		t(Std.is(cls1.date, Date),pos());
		eq(cls1.date.getTime(), new Date(2012, 7, 30, 0, 0, 0).getTime(),pos());

		t(Std.is(cls1.binary, Bytes),pos());
		eq(cls1.binary.compare(Bytes.ofString("\x01\n\r\x02")), 0,pos());
		t(cls1.enumFlags.has(FirstValue),pos());
		f(cls1.enumFlags.has(SecondValue),pos());
		t(cls1.enumFlags.has(ThirdValue),pos());

		t(Std.is(cls1.data, Array),pos());
		t(Std.is(cls1.data[0], ComplexClass),pos());

		eq(cls1.data[0].val.name, "test",pos());
		eq(cls1.data[0].val.array.length, 4,pos());
		eq(cls1.data[0].val.array[1], "is",pos());

		eq(cls1.relation.name, "first spod",pos());
		eq(cls1.relationNullable.name, "second spod",pos());

		eq(cls1.anEnum, SecondValue,pos());
		t(Std.is(cls1.anEnum, SpodEnum),pos());

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
