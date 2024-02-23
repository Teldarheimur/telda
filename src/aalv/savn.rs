use std::{
    fs::File, io::{self, BufReader, ErrorKind, Read, Result}, mem, path::Path
};

use super::AalvReader;

const SAVN_MAGIC: &str = "álvasavn\n";

pub fn read_archive<P: AsRef<Path>>(path: P) -> Result<Iter> {
    let mut f = BufReader::new(File::open(path)?);

    let mut magic_buf = [0; SAVN_MAGIC.len()];
    f.read_exact(&mut magic_buf)?;

    if magic_buf != SAVN_MAGIC.as_bytes() {
        return Err(io::Error::new(ErrorKind::InvalidData, "could not find archive magic"));
    }

    Ok(Iter(IterInner::OpenFile(f)))
}

#[derive(Default)]
#[repr(transparent)]
pub struct Iter(IterInner);

impl Iter {
    #[inline(always)]
    pub fn next<T, F: FnOnce(&mut AalvReader<BufReader<File>>) -> Result<T>>(&mut self, load_aalv: F) -> Result<Option<T>> {
        self.0.next(load_aalv)
    }
}

#[derive(Default)]
pub enum IterInner {
    #[default]
    Empty,
    OpenFile(BufReader<File>),
}

impl IterInner {
    fn next<T, F: FnOnce(&mut AalvReader<BufReader<File>>) -> Result<T>>(&mut self, load_aalv: F) -> Result<Option<T>> {
        match mem::take(self) {
            Self::Empty => Ok(None),
            Self::OpenFile(f) => {
                let mut reader = AalvReader::new(f)?;
                let ret = load_aalv(&mut reader)?;
                let mut f = reader.end()?;

                match f.read_exact(&mut [0]) {
                    Ok(_) => {
                        let _ = f.seek_relative(-1);
                        *self = Self::OpenFile(f);
                    }
                    Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => (),
                    Err(e) => return Err(e),
                }

                Ok(Some(ret))
            }
        }
    }
}
