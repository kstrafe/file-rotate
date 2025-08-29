use crate::{Action, Trigger};

/// Rotate after a specified number of newline-terminated lines.
///
/// `LineCount` counts `\n` bytes observed since the last rotation, and
/// triggers a rotation when the configured limit is reached. The consumed
/// count includes the final newline that satisfied the limit.
pub struct LineCount {
    limit: usize,
    current: usize,
}

impl LineCount {
    /// Create a new `LineCount` trigger with zero limit (must be set via `limit`).
    pub fn new() -> Self {
        Self {
            limit: 0,
            current: 0,
        }
    }

    /// Set the number of lines after which to rotate.
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

impl Trigger for LineCount {
    type Meta = ();
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        let newlines = bytes.iter().filter(|&&b| b == b'\n').count();

        if self.current + newlines >= self.limit {
            // Find the position of the line that would exceed the limit
            let mut lines_seen = 0;
            let mut consumed = 0;

            for (i, &byte) in bytes.iter().enumerate() {
                if byte == b'\n' {
                    lines_seen += 1;
                    if self.current + lines_seen >= self.limit {
                        consumed = i + 1; // Include the newline
                        break;
                    }
                }
            }

            Action::Rotate { consumed }
        } else {
            self.current += newlines;
            Action::None
        }
    }

    fn reset(&mut self) {
        self.current = 0;
    }

    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        ()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rotates_on_nth_newline_and_consumes_including_newline() {
        let mut t = LineCount::new().limit(2);
        assert!(matches!(t.trigger(b"line1\nline2"), Action::None));
        match t.trigger(b"\nrest") {
            Action::Rotate { consumed } => assert_eq!(consumed, 1),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn no_rotate_without_enough_newlines() {
        let mut t = LineCount::new().limit(3);
        assert!(matches!(t.trigger(b"a\nb"), Action::None));
        assert!(matches!(t.trigger(b"c"), Action::None));
    }

    #[test]
    fn reset_resets_line_counter() {
        let mut t = LineCount::new().limit(1);
        assert!(matches!(t.trigger(b"foo"), Action::None));
        t.reset();
        // Still requires a newline to rotate
        assert!(matches!(t.trigger(b"bar"), Action::None));
        match t.trigger(b"\n") {
            Action::Rotate { consumed } => assert_eq!(consumed, 1),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn zero_limit_immediate_rotate_zero_consumed() {
        let mut t = LineCount::new().limit(0);
        match t.trigger(b"no_newlines_here") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected immediate rotate for zero limit"),
        }
    }

    #[test]
    fn one_limit_consumes_first_newline() {
        let mut t = LineCount::new().limit(1);
        match t.trigger(b"foo\nbar") {
            Action::Rotate { consumed } => assert_eq!(consumed, 4),
            _ => panic!("expected rotate on first newline"),
        }
    }

    #[test]
    fn multiple_rotations_across_buffers() {
        let mut t = LineCount::new().limit(1);
        let buf = b"a\nb\n";
        // First rotation
        match t.trigger(buf) {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected first rotate"),
        }
        t.reset();
        // Remaining bytes
        let rem = &buf[2..];
        match t.trigger(rem) {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected second rotate"),
        }
    }
}
