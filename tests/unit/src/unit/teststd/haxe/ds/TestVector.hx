package unit.teststd.haxe.ds;

class TestVector extends unit.Test {
	public function test() {
		var vec = new haxe.ds.Vector(3);
		var vNullInt = #if static 0 #else null #end;
		var vNullBool = #if static false #else null #end;
		var vNullFloat = #if static 0.0 #else null #end;
		eq(vec.length, 3);
		eq(vec.get(0), vNullInt);
		eq(vec.get(1), vNullInt);
		eq(vec.get(2), vNullInt);
		vec.set(1, 2);
		eq(vec.length, 3);
		eq(vec.get(0), vNullInt);
		eq(vec.get(1), 2);
		eq(vec.get(2), vNullInt);

		// float init
		var vec = new haxe.ds.Vector<Float>(3);
		eq(vec.get(0), vNullFloat);
		eq(vec.get(1), vNullFloat);
		eq(vec.get(2), vNullFloat);

		// bool init
		var vec = new haxe.ds.Vector<Bool>(3);
		eq(vec.get(0), vNullBool);
		eq(vec.get(1), vNullBool);
		eq(vec.get(2), vNullBool);

		// fromArray
		var arr = ["1", "2", "3"];
		var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(arr);
		#if (!flash && !neko && !jvm && !lua && !eval && !php)
		t(arr != vec.toData());
		#end
		eq(vec.length, 3);
		eq(vec.get(0), "1");
		eq(vec.get(1), "2");
		eq(vec.get(2), "3");

		// toArray
		var vec = new haxe.ds.Vector(3);
		vec.set(1, 2);
		var arr = vec.toArray();
		eq(arr[0], vNullInt);
		eq(arr[1], 2);
		eq(arr[3], vNullInt);

		// objects
		var tpl = new C();
		var vec:haxe.ds.Vector<C> = haxe.ds.Vector.fromArrayCopy([tpl]);
		eq(tpl, vec.get(0));

		// toData + fromData
		var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(["1", "2", "3"]);
		var data = vec.toData();
		var vec2 = haxe.ds.Vector.fromData(data);
		eq(vec2.get(0), "1");
		eq(vec2.get(1), "2");
		eq(vec2.get(2), "3");

		// []
		aeq(["1", "2", "3"], vec2.toArray());
		vec2[1] = "4";
		eq(vec2[1], "4");
		vec2[0] += "a";
		vec2[0] = "1a";

		// blit
		var vec3 = haxe.ds.Vector.fromArrayCopy([0,1,2,3,4,5,6]);
		var vec4 = new haxe.ds.Vector(5);

		haxe.ds.Vector.blit(vec3, 0, vec4, 1, 3);
		eq(vec4[1], 0);
		eq(vec4[2], 1);
		eq(vec4[3], 2);
		eq(vec4[4], vNullInt);
		eq(vec4[0], vNullInt);

		haxe.ds.Vector.blit(vec3, 0, vec4, 0, 5);
		aeq([0, 1, 2, 3, 4], vec4.toArray());

		haxe.ds.Vector.blit(vec4, 1, vec3, 0, 4);
		//vec3 should be [1,2,3,4,4,5,6]
		aeq([1, 2, 3, 4, 4, 5, 6], vec3.toArray());

		var vec5 = new haxe.ds.Vector(3, 5);
		aeq([5, 5, 5], vec5.toArray());
		vec5.fill(1);
		aeq([1, 1, 1], vec5.toArray());

		var vec5 = new haxe.ds.Vector(3, true);
		t(vec5[0]);
		t(vec5[1]);
		t(vec5[2]);

		var vec5 = haxe.ds.Vector.fromArrayCopy([0,1,2,3,4]);
		haxe.ds.Vector.blit(vec5, 0, vec5, 1, 4);
		aeq([0, 0, 1, 2, 3], vec5.toArray());

		var vec5 = haxe.ds.Vector.fromArrayCopy([0,1,2,3,4]);
		haxe.ds.Vector.blit(vec5, 1, vec5, 0, 4);
		aeq([1, 2, 3, 4, 4], vec5.toArray());

		var vec5 = haxe.ds.Vector.fromArrayCopy([0,1,2,3,4]);
		haxe.ds.Vector.blit(vec5, 0, vec5, 0, 5);
		aeq([0, 1, 2, 3, 4], vec5.toArray());

		// test iteration

		var vec1 = new haxe.ds.Vector(2);
		vec1[0] = 1;
		vec1[1] = 2;
		var res = 0;
		for (e in vec1) {
			res += e;
		}
		eq(3, res);

		// copy

		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);

		var vec = new haxe.ds.Vector(3);
		vec[0] = i0;
		vec[1] = i1;
		vec[2] = i2;
		var vec2 = vec.copy();
		f(vec == vec2);
		aeq([vec2[0], vec2[1], vec2[2]], vec.toArray());

		// join

		var vec = new haxe.ds.Vector(0);
		eq(vec.join(","), "");

		var vec = new haxe.ds.Vector(1);
		eq(vec.join(","), "null");

		var vec = new haxe.ds.Vector(2);
		eq(vec.join(","), "null,null");

		var vec = new haxe.ds.Vector(2);
		vec[0] = "foo";
		vec[1] = "bar";
		eq(vec.join(", "), "foo, bar");


		// map

		var vec = new haxe.ds.Vector(0);
		vec.map(function(i) {
			throw false;
			return null;
		});

		var vec = new haxe.ds.Vector(2);
		vec[0] = 12;
		vec[1] = 13;
		var vec2 = vec.map(function(i) return "value: " +i);
		aeq(["value: 12", "value: 13"], vec2.toArray());

		// sort

		#if !(neko || jvm || eval)
		var vec = new haxe.ds.Vector(4);
		vec[0] = 99;
		vec[1] = 101;
		vec[2] = -12;
		vec[3] = 0;
		vec.sort(Reflect.compare);
		aeq([-12, 0, 99, 101], vec.toArray());
		#end

	}
}
