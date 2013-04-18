use rust::Lib;
mod HxObject;
pub struct List<T> {
	length: i32, 
	q: Option<~[@HxObject]>, 
	h: Option<~[@HxObject]>
}
pub impl<T> List<T> {
	pub fn iterator(&mut self) -> @HxObject {
		return { h : (rust::Lib::unwrap(self)).h, hasNext : Some(@|| -> @HxObject {
			return (rust::Lib::unwrap(self)).h != None;
		}), next : Some(@|| -> @HxObject {
			if ((rust::Lib::unwrap(self)).h == None) {
				return None;
			}
			let mut x: @HxObject = (rust::Lib::unwrap(self)).h[0i32];
			{
				(rust::Lib::unwrap(self)).h = (rust::Lib::unwrap(self)).h[1i32];
				(rust::Lib::unwrap(self)).h
			}
			return x;
		})}
	}
	pub fn add(&mut self, item: Option<@T>) -> () {
		let mut x: Option<~[@HxObject]> = Some(~[item]);
		if ((rust::Lib::unwrap(self)).h == None) {
			{
				(rust::Lib::unwrap(self)).h = x;
				(rust::Lib::unwrap(self)).h
			}
		}
		else {
			{
				(rust::Lib::unwrap(self)).q[1i32] = x;
				(rust::Lib::unwrap(self)).q[1i32]
			}
		}
		{
			(rust::Lib::unwrap(self)).q = x;
			(rust::Lib::unwrap(self)).q
		}
		{
			(rust::Lib::unwrap(self)).length += 1;
			(rust::Lib::unwrap(self)).length - 1
		};
		}
	pub fn new() -> Option<@List> {
		let mut self = List {length: 0i32, q: None, h: None}
		{
			(rust::Lib::unwrap(self)).length = 0i32;
			(rust::Lib::unwrap(self)).length
		}
		return Some(@self);
	}
}
impl HxObject for List<T> {
}
