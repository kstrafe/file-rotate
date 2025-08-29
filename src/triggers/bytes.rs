/// Rotate when the total number of bytes written reaches a configured limit.
///
/// `Bytes` maintains a simple counter of how many bytes have been seen since the
/// last rotation and requests a rotation as soon as the threshold is met or
/// exceeded. The `consumed` count returned in `Action::Rotate` indicates the
/// number of bytes from the current buffer that were written before rotation.
use crate::{Action, Trigger};

pub struct Bytes {
    pub count: usize,
    pub limit: usize,
}

impl Bytes {
    /// Create a new `Bytes` trigger with a default limit of 1 MiB.
    pub fn new() -> Self {
        let byte_count_1_mib = 1_048_576;
        Self {
            count: 0,
            limit: byte_count_1_mib,
        }
    }

    /// Set the byte limit at which a rotation is triggered.
    pub fn limit(mut self, bytes: usize) -> Self {
        self.limit = bytes;
        self
    }
}

impl Default for Bytes {
    fn default() -> Self {
        Self::new()
    }
}

impl Trigger for Bytes {
    type Meta = ();
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        if self.count + bytes.len() >= self.limit {
            let consumed = self.limit - self.count;
            Action::Rotate { consumed }
        } else {
            self.count += bytes.len();
            Action::None
        }
    }

    fn reset(&mut self) {
        self.count = 0;
    }

    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        ()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_rotate_below_limit() {
        let mut t = Bytes::new().limit(5);
        assert!(matches!(t.trigger(b"abc"), Action::None));
        assert!(matches!(t.trigger(b"d"), Action::None));
    }

    #[test]
    fn rotate_on_reaching_limit_and_report_consumed() {
        let mut t = Bytes::new().limit(5);
        // First 3 bytes do not rotate
        assert!(matches!(t.trigger(b"abc"), Action::None));
        // Next 4 bytes should rotate after consuming 2 to reach 5
        match t.trigger(b"defg") {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn reset_clears_internal_count() {
        let mut t = Bytes::new().limit(3);
        assert!(matches!(t.trigger(b"ab"), Action::None));
        t.reset();
        // Should require full 3 bytes again after reset
        match t.trigger(b"abc") {
            Action::Rotate { consumed } => assert_eq!(consumed, 3),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn zero_limit_rotates_immediately_with_zero_consumed() {
        let mut t = Bytes::new().limit(0);
        match t.trigger(b"hello") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected immediate rotate"),
        }
    }

    #[test]
    fn one_limit_consumes_one() {
        let mut t = Bytes::new().limit(1);
        match t.trigger(b"abc") {
            Action::Rotate { consumed } => assert_eq!(consumed, 1),
            _ => panic!("expected rotate at one"),
        }
    }

    #[test]
    fn multiple_rotations_progressively() {
        let mut t = Bytes::new().limit(2);
        // First rotation in first buffer
        match t.trigger(b"ab") {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected rotate"),
        }
        t.reset();
        // Second rotation in next buffer
        match t.trigger(b"cd") {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected rotate"),
        }
    }
}
