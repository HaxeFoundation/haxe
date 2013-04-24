mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		let mut a: i32 = 0i32, b: f32 = f32::NaN, c: i8 = 0i8;
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}}
