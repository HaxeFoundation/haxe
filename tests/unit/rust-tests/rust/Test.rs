extern mod std;
mod lib;
mod STest;
pub struct Test<V> {
	value: Option<~V>
	
}
pub impl<V> Test<V> {
	pub fn main() -> () {
		let mut ot: Option<~Test<~lib::HxObject>> = Test::new(None);
		let mut t: Option<~lib::HxObject> = {
			let mut o = ~std::treemap::TreeMap::new();
			o.insert(~"id", Some(~"Goodbye, world!"));
			o.insert(~"value", 23i32);
			Some(~o)
		};
		let mut oi: i8 = (24i32 as i8);
		oi += {
			let mut t: i32 = 5i32;
			if (Std::_is(t, i8.unwrap())) {
				t;
			} else {
				fail!(~"Class cast error");
			};
			(t as i8);
		};
		io::println(Some(Some(~"hello, world!") + (&(t.unwrap().value) as &lib::HxObject).toString()).unwrap());
	}
	pub fn new(v: Option<~V>) -> Option<~Test> {
		let mut self = Test {value: None};;
		self.value = v;
		return Some(~self);
	}
}
impl<V> STest::STest for Test<V> {
}
impl<V> lib::HxObject for Test<V> {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Test")
	}
}
