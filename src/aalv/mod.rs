use std::{io::{self, Write, BufRead, Seek, ErrorKind, BufReader, SeekFrom, Result, Read, BufWriter}, path::Path, fs::File};

const AALV_MAGIC: &str = "álvur2\n";

pub fn read_aalv_file<P: AsRef<Path>>(path: P) -> Result<AalvReader<BufReader<File>>> {
    let f = BufReader::new(File::open(path)?);
    AalvReader::new(f)
}

pub fn write_aalv_file<P: AsRef<Path>>(path: P) -> Result<AalvWriter<BufWriter<File>>> {
    write_aalv_file_with_offset(path, 0)
}

pub fn write_aalv_file_with_offset<P: AsRef<Path>>(path: P, file_offset: u64) -> Result<AalvWriter<BufWriter<File>>> {
    let f = File::options().create(true).write(true).open(path)?;
    f.set_len(file_offset)?;
    AalvWriter::new(BufWriter::new(f), file_offset)
}

pub struct AalvReader<F> {
    pub file_offset: u64,
    file: F,
    sections: Vec<(Box<str>, u64, u16)>,
}

impl<F: BufRead + Seek> AalvReader<F> {
    pub fn new(mut file: F) -> Result<Self> {
        let mut new = Self {
            file_offset: file.stream_position()?,
            file,
            sections: Vec::new(),
        };
        new.read_magic()
            .map_err(|e| {
                if e.kind() == io::ErrorKind::UnexpectedEof {
                    io::Error::new(ErrorKind::InvalidData, "could not find magic")
                } else {
                    e
                }
            })?;
        new.read_section_headers()?;

        Ok(new)
    }

    pub fn read_section<S: Section>(&mut self) -> Option<Result<S>> {
        let id = self.sections
            .iter()
            .position(|(s, _pos, _size)| &**s == S::NAME)?;

        let (_, pos, size) = self.sections.remove(id);
        match self.file.seek(SeekFrom::Start(pos)) {
            Ok(_) => (),
            Err(e) => return Some(Err(e)),
        }

        let read_window = ReadWindow {
            reader: &mut self.file,
            length: size as usize,
        };
        Some(S::read(read_window))
    }
    pub fn remaing_sections(&self) -> impl Iterator<Item=&str> {
        self.sections
            .iter()
            .map(|(s, _, _)| &**s)
    }

    fn read_magic(&mut self) -> Result<()> {
        // Find magic
        loop {
            let first_magic_byte = AALV_MAGIC.as_bytes()[0];
            let mut magic_buf = [0; AALV_MAGIC.len()];

            while first_magic_byte != magic_buf[0] {
                self.file.read_exact(&mut magic_buf[..1])?;
            }
            self.file_offset = self.file.stream_position()? - 1;

            self.file.read_exact(&mut magic_buf[1..])?;
            if magic_buf == AALV_MAGIC.as_bytes() {
                break;
            }
        }

        Ok(())
   }

   fn read_section_headers(&mut self) -> Result<()> {
        let mut name_buf = Vec::new();
        loop {
            name_buf.clear();
            self.file.read_until(b'\0', &mut name_buf)?;

            if name_buf.pop() != Some(0) {
                return Err(io::Error::new(ErrorKind::InvalidData, "name did not end in a zero byte"));
            }

            let name: Box<str> = String::from_utf8_lossy(&name_buf).into();

            if name.len() == 0 {
                break;
            }

            let mut num_buf = [0; 2];
            self.file.read_exact(&mut num_buf)?;

            let len = u16::from_le_bytes(num_buf);

            let pos = self.file.stream_position()?;

            self.file.seek(SeekFrom::Current(len as i64))?;

            self.sections.push((name, pos, len));
        }

        Ok(())
   }
}

struct ReadWindow<R: Read> {
    reader: R,
    length: usize,
}

impl<R: Read> Read for ReadWindow<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> Result<usize> {
        if buf.len() > self.length {
            buf = &mut buf[..self.length];
        }

        let n = self.reader.read(buf)?;
        self.length -= n;
        Ok(n)
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
        if buf.len() > self.length {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "read window is smaller"));
        }

        self.reader.read_exact(buf)?;
        self.length -= buf.len();
        Ok(())
    }
}

pub struct AalvWriter<F: Write> {
    file: F,
}

impl<F: Write + Seek> AalvWriter<F> {
    pub fn new(file: F, file_offset: u64) -> Result<Self> {
        let mut new = AalvWriter { file };

        new.file.seek(SeekFrom::Start(file_offset))?;
        new.write_magic()?;
        Ok(new)
    }

    fn write_magic(&mut self) -> Result<()> {
        self.file.write_all(AALV_MAGIC.as_bytes())
    }

    pub fn write_section<S: Section>(&mut self, segment: &S) -> Result<()> {
        assert!(!S::NAME.contains('\0'), "section name may not contain null bytes");
        assert_ne!(!S::NAME.len(), 0, "section name may not be empty");

        write!(self.file, "{}\0\0\0", S::NAME)?;

        let mut w = CountedWriter {
            writer: &mut self.file,
            length: 0,
        };

        segment.write(&mut w)?;
        let CountedWriter { writer: _, length } = w;

        self.file.seek(SeekFrom::Current(-(length as i64) - 2))?;
        self.file.write_all(&(length as u16).to_le_bytes())?;
        self.file.seek(SeekFrom::Current(length as i64))?;

        Ok(())
    }
}

impl<F: Write> Drop for AalvWriter<F> {
    fn drop(&mut self) {
        self.file.write_all(&[0]).unwrap();
    }
}

struct CountedWriter<W: Write> {
    writer: W,
    length: usize,
}

impl<R: Write> Write for CountedWriter<R> {
    fn flush(&mut self) -> Result<()> {
        self.writer.flush()
    }
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let n = self.writer.write(buf)?;
        self.length += n;
        Ok(n)
    }
    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        self.length += buf.len();
        self.writer.write_all(buf)
    }
}

pub trait Section: Sized {
    const NAME: &'static str;
    fn read<R: Read>(reader: R) -> Result<Self>;
    fn write<W: Write>(&self, writer: W) -> Result<()>;
}

pub mod obj;
pub mod sample {
    use super::Section;
    use std::io::{Write, Read, Result};

    #[repr(transparent)]
    pub struct Name(pub String);
    
    impl Section for Name {
        const NAME: &'static str = "name";

        fn read<R: Read>(mut reader: R) -> Result<Self> {
            let mut buf = String::new();
            reader.read_to_string(&mut buf)?;
            Ok(Name(buf))
        }
        fn write<W: Write>(&self, mut writer: W) -> Result<()> {
            write!(writer, "{}", self.0)
        }
    }
}
