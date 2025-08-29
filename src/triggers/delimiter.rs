use crate::{Action, Trigger};

/// Rotate when a specific delimiter byte sequence is encountered.
///
/// The delimiter can span multiple writes. Internal progress is tracked so
/// that partial matches across buffer boundaries are handled correctly. When a
/// rotation occurs, the trigger state is reset (including when used via the
/// combinator), ensuring that partial matches do not leak across rotations.
pub struct Delimiter {
    pat: Vec<u8>,
    include: bool,
    progress: usize,
}

impl Delimiter {
    /// Create a new `Delimiter` trigger.
    ///
    /// If `include` is `true`, the bytes consumed for rotation include the
    /// delimiter itself; otherwise, the rotation point is positioned just
    /// before the delimiter so the delimiter appears at the start of the next
    /// file.
    pub fn new<P: AsRef<[u8]>>(pattern: P, include: bool) -> Self {
        Self {
            pat: pattern.as_ref().to_vec(),
            include,
            progress: 0,
        }
    }
}

impl Trigger for Delimiter {
    type Meta = ();
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        if self.pat.is_empty() {
            return Action::None;
        }
        let m = self.pat.len();
        let start_progress = self.progress;
        for (i, &b) in bytes.iter().enumerate() {
            while self.progress > 0 && self.pat[self.progress] != b {
                self.progress = 0;
            }
            if self.pat[self.progress] == b {
                self.progress += 1;
                if self.progress == m {
                    let consumed = if self.include {
                        i + 1
                    } else {
                        let excl = i + 1;
                        let delim_from_this = m.saturating_sub(start_progress);
                        excl.saturating_sub(delim_from_this)
                    };
                    return Action::Rotate { consumed };
                }
            }
        }
        Action::None
    }

    fn reset(&mut self) {
        self.progress = 0;
    }

    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        ()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rotates_including_delimiter() {
        let mut t = Delimiter::new(b"\n", true);
        match t.trigger(b"abc\ndef") {
            Action::Rotate { consumed } => assert_eq!(consumed, 4),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn rotates_excluding_delimiter() {
        let mut t = Delimiter::new(b"###", false);
        match t.trigger(b"abc###def") {
            Action::Rotate { consumed } => assert_eq!(consumed, 3),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn cross_chunk_match_excluding_delimiter_consumes_zero_from_second_chunk() {
        let mut t = Delimiter::new(b"XYZ", false);
        // Partial match advances progress
        assert!(matches!(t.trigger(b"XY"), Action::None));
        // Completing match in next chunk should consume 0 from this chunk
        match t.trigger(b"Z123") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn reset_clears_partial_progress() {
        let mut t = Delimiter::new(b"XYZ", false);
        assert!(matches!(t.trigger(b"XY"), Action::None));
        t.reset();
        // No longer completes a delimiter
        assert!(matches!(t.trigger(b"Z"), Action::None));
    }

    #[test]
    fn multiple_rotations_with_include_across_buffer() {
        let buf = b"a\nb\nc";
        let mut t = Delimiter::new(b"\n", true);
        match t.trigger(buf) {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected first rotate"),
        }
        t.reset();
        // Remaining bytes after first rotation
        let rem = &buf[2..]; // b"b\nc"
        match t.trigger(rem) {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected second rotate"),
        }
        t.reset();
        // Remaining after second rotation
        let rem2 = &rem[2..]; // b"c"
        assert!(matches!(t.trigger(rem2), Action::None));
    }

    #[test]
    fn multiple_rotations_with_exclude_across_buffer() {
        let buf = b"abc###def###ghi";
        let mut t = Delimiter::new(b"###", false);
        match t.trigger(buf) {
            Action::Rotate { consumed } => assert_eq!(consumed, 3),
            _ => panic!("expected first rotate"),
        }
        t.reset();
        let rem = &buf[3 + 3..]; // skip "abc###" => b"def###ghi"
        match t.trigger(rem) {
            Action::Rotate { consumed } => assert_eq!(consumed, 3),
            _ => panic!("expected second rotate"),
        }
        t.reset();
        let rem2 = &rem[3 + 3..]; // b"ghi"
        assert!(matches!(t.trigger(rem2), Action::None));
    }

    #[test]
    fn empty_pattern_never_triggers() {
        let mut t = Delimiter::new(b"", true);
        assert!(matches!(t.trigger(b"anything\n"), Action::None));
        t.reset();
        assert!(matches!(t.trigger(b""), Action::None));
    }
}
