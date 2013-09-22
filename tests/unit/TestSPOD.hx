package unit;
import haxe.io.Bytes;
import haxe.EnumFlags;
import sys.db.Connection;
import sys.db.Manager;
import sys.db.Sqlite;
import sys.db.TableCreate;
import sys.FileSystem;
import unit.SPODClasses;

class TestSPOD extends Test
{
	public function new()
	{
		super();
		if (FileSystem.exists("db.db3"))
			FileSystem.deleteFile("db.db3");

		var db = sys.db.Sqlite.open("db.db3");
		Manager.cnx = db;
		Manager.initialize();

		TableCreate.create(MySpodClass.manager);
		TableCreate.create(OtherSpodClass.manager);
	}

	public function testSpodFail()
	{
		exc(function() new MySpodClass().insert());
		exc(function() new OtherSpodClass(null).insert());
	}

  function getDefaultClass()
  {
    var scls = new MySpodClass();
    scls.int = 1;
    scls.double = 2.0;
    scls.boolean = true;
    scls.string = "some string";
    scls.date = new Date(2012, 07, 30, 0, 0, 0);

    var bytes = Bytes.ofString("\x01\n\r\x02");
    scls.binary = bytes;
    scls.enumFlags = EnumFlags.ofInt(0);
    scls.enumFlags.set(FirstValue);
    scls.enumFlags.set(ThirdValue);

    scls.data = [new ComplexClass( { name:"test", array:["this", "is", "a", "test"] } )];
    // scls.anEnum = SecondValue;

    return scls;
  }

	public function testSpodTypes()
	{
		var c1 = new OtherSpodClass("first spod");
		c1.insert();
		var c2 = new OtherSpodClass("second spod");
		c2.insert();

		var scls = getDefaultClass();

		scls.relation = c1;
		scls.relationNullable = c2;
		scls.insert();

		//after inserting, id must be filled
		t(scls.theId != 0 && scls.theId != null);
		var theid = scls.theId;

		c1 = c2 = null;
		Manager.cleanup();

		var cls1 = MySpodClass.manager.get(theid);
		t(cls1 != null);
		//after Manager.cleanup(), the instances should be different
		f(cls1 == scls);
		scls = null;

		t(Std.is(cls1.int, Int));
		eq(cls1.int, 1);
		t(Std.is(cls1.double, Float));
		eq(cls1.double, 2.0);
		t(Std.is(cls1.boolean, Bool));
		eq(cls1.boolean, true);
		t(Std.is(cls1.string, String));
		eq(cls1.string, "some string");
		t(cls1.date != null);
		t(Std.is(cls1.date, Date));
		eq(cls1.date.getTime(), new Date(2012, 07, 30, 0, 0, 0).getTime());

		t(Std.is(cls1.binary, Bytes));
		eq(cls1.binary.compare(Bytes.ofString("\x01\n\r\x02")), 0);
		t(cls1.enumFlags.has(FirstValue));
		f(cls1.enumFlags.has(SecondValue));
		t(cls1.enumFlags.has(ThirdValue));

		t(Std.is(cls1.data, Array));
		t(Std.is(cls1.data[0], ComplexClass));

		eq(cls1.data[0].val.name, "test");
		eq(cls1.data[0].val.array.length, 4);
		eq(cls1.data[0].val.array[1], "is");

		eq(cls1.relation.name, "first spod");
		eq(cls1.relationNullable.name, "second spod");

		//test create a new class
		var scls = getDefaultClass();

		c1 = new OtherSpodClass("third spod");
		c1.insert();

		scls.relation = c1;
		scls.insert();

		scls = cls1 = null;
		Manager.cleanup();

		eq(2, MySpodClass.manager.all().length);
		var req = MySpodClass.manager.search({ relation: OtherSpodClass.manager.select({ name:"third spod"} ) });
		eq(req.length, 1);
		scls = req.first();

		scls.relation.name = "Test";
		scls.relation.update();

		eq(OtherSpodClass.manager.select({ name:"third spod" }), null);

		for (c in MySpodClass.manager.all())
			c.delete();
		for (c in OtherSpodClass.manager.all())
			c.delete();
	}

	public function testDateQuery()
	{
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
		eq(q.length, 1);
		eq(q.first(), c2);

		q = MySpodClass.manager.search($date == now);
		eq(q.length, 1);
		eq(q.first(), c1);

		q = MySpodClass.manager.search($date >= now);
		eq(q.length, 2);
		eq(q.first(), c1);

		q = MySpodClass.manager.search($date >= DateTools.delta(now, DateTools.hours(2)));
		eq(q.length, 0);
		eq(q.first(), null);

		c1.delete();
		c2.delete();
		other1.delete();
	}

	public function testData()
	{
		var other1 = new OtherSpodClass("required field");
		other1.insert();

		var c1 = getDefaultClass();
		c1.relation = other1;
		c1.insert();

		eq(c1.data.length,1);
		c1.data.pop();
		c1.update();

		Manager.cleanup();
		c1 = null;

		c1 = MySpodClass.manager.select($relation == other1);
		eq(c1.data.length, 0);
		c1.data.push(new ComplexClass({ name: "test1", array:["complex","field"] }));
		c1.data.push(null);
		eq(c1.data.length, 2);
		c1.update();

		Manager.cleanup();
		c1 = null;

		c1 = MySpodClass.manager.select($relation == other1);
		eq(c1.data.length,2);
		eq(c1.data[0].val.name, "test1");
		eq(c1.data[0].val.array.length, 2);
		eq(c1.data[0].val.array[0], "complex");
		eq(c1.data[1], null);

		c1.delete();
		other1.delete();
	}
}

