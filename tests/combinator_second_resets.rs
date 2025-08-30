use std::cell::Cell;
use std::io::Write;
use std::rc::Rc;

use file_rotate::{
    rotators::MemoryRotator,
    triggers::{Bytes, TriggerExt},
    FileRotate, Trigger,
};

#[derive(Clone)]
struct ResetSpy(Rc<Cell<usize>>);

impl Trigger for ResetSpy {
    type Meta = ();
    fn trigger(&mut self, _bytes: &[u8]) -> file_rotate::Action {
        file_rotate::Action::None
    }
    fn reset(&mut self) {
        self.0.set(self.0.get() + 1);
    }
    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        // No metadata
    }
}

#[test]
fn first_trigger_resets_when_second_fires() {
    let resets = Rc::new(Cell::new(0));
    let spy = ResetSpy(resets.clone());

    let rotator = MemoryRotator::new();
    // First trigger is spy (never fires); second will fire.
    let combined = spy.or(Bytes::new().limit(5));
    let mut fr = FileRotate::new(rotator, combined).unwrap();

    write!(fr, "hello").unwrap(); // triggers bytes at 5 bytes

    assert_eq!(
        resets.get(),
        1,
        "spy (first) should have been reset once after rotation"
    );
}
