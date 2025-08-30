use crate::Rotator;
use std::io::{self, Write};

/// In-memory rotator useful for tests and examples.
///
/// Each rotation finalizes the current buffer and starts a new one in `buffers`.
/// Buffers can be inspected to assert rotation behavior without touching the
/// filesystem.
pub struct MemoryRotator {
    pub(crate) buffers: Vec<Vec<u8>>,
}

impl MemoryRotator {
    /// Create an empty memory rotator.
    pub fn new() -> Self {
        Self {
            buffers: Vec::new(),
        }
    }

    /// Returns the buffer for a given rotation index, if present.
    pub fn get_buffer(&self, index: usize) -> Option<&[u8]> {
        self.buffers.get(index).map(|v| v.as_slice())
    }

    /// Returns all buffers accumulated so far.
    pub fn all_buffers(&self) -> &[Vec<u8>] {
        &self.buffers
    }
}

impl Default for MemoryRotator {
    fn default() -> Self {
        Self::new()
    }
}

impl Rotator for MemoryRotator {
    type Writer = MemoryWriter;

    fn initial(&mut self) -> io::Result<Self::Writer> {
        self.buffers.push(Vec::new());
        Ok(MemoryWriter::new(self.buffers.len() - 1))
    }

    fn rotate(&mut self, current: Self::Writer) -> io::Result<Self::Writer> {
        if let Some(buffer) = self.buffers.get_mut(current.index) {
            *buffer = current.buffer;
        }
        self.buffers.push(Vec::new());
        Ok(MemoryWriter::new(self.buffers.len() - 1))
    }
}

/// Writer for `MemoryRotator` that accumulates bytes in a `Vec<u8>`.
pub struct MemoryWriter {
    pub(crate) buffer: Vec<u8>,
    pub(crate) index: usize,
}

impl MemoryWriter {
    /// Create a new memory writer referencing a particular buffer index.
    fn new(index: usize) -> Self {
        Self {
            buffer: Vec::new(),
            index,
        }
    }
}

impl Write for MemoryWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn rotates_and_stores_buffers() {
        let mut rot = MemoryRotator::new();
        let mut w = rot.initial().unwrap();
        write!(w, "abc").unwrap();
        let mut w = rot.rotate(w).unwrap();
        assert_eq!(rot.get_buffer(0).unwrap(), b"abc");

        write!(w, "def").unwrap();
        let _w = rot.rotate(w).unwrap();
        assert_eq!(rot.get_buffer(1).unwrap(), b"def");

        // There should be three buffers total (including the current empty one)
        assert_eq!(rot.all_buffers().len(), 3);
    }
}
