pub trait Boxable {
    fn boxed(self) -> Box<Self>;
}
