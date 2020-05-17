use eller::element;

struct Foo;

impl Foo {
    fn builder() -> Self {
        Foo
    }

    fn build(self) -> Self {
        self
    }
}

fn main() {
    let x = element!(
        <Foo />
    );
}
