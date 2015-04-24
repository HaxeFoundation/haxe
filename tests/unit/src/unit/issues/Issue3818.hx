package unit.issues;

class Issue3818 extends Test
{
	public function test()
	{
		var m = new Map<String, Int>();
		var ss = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];

		// NOTE: these three keys hash to the same value
		var s1 = "UI/fx/Materials/fx_ui_atlas_icons_additive_mat";
		var s2 = "UI/img/bg_info";
		var s3 = "art/3d/env/props/city_props/textures/cityprops_debris01";
#if java
		//known collisions
		s1 = "Aa";
		s2 = "BB";
		s3 = "Ba";
#end

		// prime to resize the hash array to be big enough
		for (s in ss) {
			m[s] = 0;
		}

		// make colliding entries
		m[s1] = 1;
		m[s2] = 2;
		m[s3] = 3;

		m.remove(s1);
		// colliding entries at this point: [DELETED, s2, s3]

		m[s2] = 4;
		// colliding entries at this point: [s2, s2, s3] (!!!)

		m.remove(s2);
		// [DELETED, s2, s3]

		f(m.exists(s2)); // ERROR OMGZ
	}
}
