mod lib;
mod Sys;
pub struct Test;
impl Test {
	pub fn main() -> () {
		let mut args: Option<~[Option<~str>]> = Sys::Sys::args();
		match ((args.unwrap().len() as i32)) {
			2i32 => Some(match (args.unwrap()[0]).unwrap() {
				~"fib" => {
					let mut n: Option<~str> = args.unwrap()[1];
					{
						let mut r: i32 = 1;
						let mut l: i32 = 0;
						{
							let mut _g1: i32 = 1;
							let mut _g: i32 = i32::from_str(n.unwrap());
							while ((_g1 < _g)) {
								let mut i: i32 = {
									_g1 += 1;
									_g1 - 1
								};
								r += (i + l);
								l = r;
							};
						};
						(println)(Some(Some(Some(Some(~"Fib(") + n) + Some(~") = ")) + (&(r) as &lib::HxObject).toString()).unwrap())
					}
				}.unwrap(),
				_ => fail!(~"Invalid arguments")
			}),
			1i32 => {
				let mut z: Option<~str> = args.unwrap()[0];
				(println)(Some(Some(Some(~"Hello, ") + z) + Some(~"!")).unwrap())
			}.unwrap(),
			_ => fail!(~"Invalid arguments")
		};
	}
}
impl lib::HxObject for ~Test {
	fn toString(&self) -> Option<~str> {
		return Some(~"Test: { }");
	}
}
