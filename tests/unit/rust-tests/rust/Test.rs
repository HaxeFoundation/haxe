mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		let mut func: @fn(i32)->i32 = (|n: i32| -> i32 {
			n + 1i32
		});
		func(34i32);
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}
}
