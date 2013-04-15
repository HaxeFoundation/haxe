mod HxObject;
mod Item;
pub struct SubTest;
pub impl SubTest {
	pub fn new() -> Option<@SubTest> {
		let mut self = SubTest {}
		self.super()(Item.value(87i32));
		return @Some(self);
	}
}
impl HxObject for SubTest {
}
