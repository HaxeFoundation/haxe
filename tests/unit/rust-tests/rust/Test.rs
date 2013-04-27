mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		let mut a: Option<i32> = Some(46i32);
		let mut func: @fn(i32)->Option<i32> = (|a:i32|->Option<i32>{ None });
		func = (|n: i32| -> Option<i32> {
			Some(n)
		});
		func(24i32);
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}
}
