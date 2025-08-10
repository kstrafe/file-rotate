use super::*;

pub struct Bytes {
    pub count: usize,
    pub limit: usize,
}

impl Bytes {
    pub fn new() -> Self {
        let byte_count_1_mib = 1048576;
        Self {
            count: 0,
            limit: byte_count_1_mib,
        }
    }

    pub fn limit(mut self, bytes: usize) -> Self {
        self.limit = bytes;
        self
    }
}

impl Trigger for Bytes {
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        if self.count + bytes.len() > self.limit {
            let consumed = self.limit - self.count;
            self.reset();
            Action::Rotate { consumed }
        } else {
            self.count += bytes.len();
            Action::None
        }
    }

    fn reset(&mut self) {
        self.count = 0;
    }
}
