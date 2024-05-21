//! Definitions for Rubik's cubes.

use crate::group::{ Move, Seq };

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Cube([usize; 54]);

impl Default for Cube {
    fn default() -> Self { Self::new() }
}

impl Cube {
    pub fn new() -> Self {
        let mut data = [0; 54];
        data.iter_mut().enumerate().for_each(|(k, d)| { *d = k; });
        Self(data)
    }

    pub fn is_solved(&self) -> bool {
        self.0.iter().enumerate().all(|(k, d)| *d == k)
    }

    pub fn permute<I>(&mut self, idx: I)
    where I: IntoIterator<Item = usize>
    {
        let idx: Vec<usize> = idx.into_iter().collect();
        if idx.len() < 2 || idx.iter().any(|&k| k >= 54) { return; }
        let mut scratch: usize = 0;
        let first: usize = idx[0];
        idx.into_iter()
            .for_each(|k| { std::mem::swap(&mut scratch, &mut self.0[k]); });
        std::mem::swap(&mut scratch, &mut self.0[first]);
    }


}

