mod lib;
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
	}
	
	pub fn new(v: Option<~V>) -> Option<~Test> {
		let mut self = Test {value: None};;
		self.value = v;
		return Some(~self);
	}
	
}
impl STest for Test<V> {
}
impl<V> lib::HxObject for Test<V> {
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Test");
	}
}
