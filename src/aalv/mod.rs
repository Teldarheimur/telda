use std::{io::{self, Write, BufRead, Seek, ErrorKind, BufReader, SeekFrom}, path::Path, fs::File};

const AALV_HEADER: &str = "álvur1";

pub fn read_aalv_file<P: AsRef<Path>>(path: P) -> io::Result<Aalv> {
    let f = BufReader::new(File::open(path)?);
    Aalv::read(f)
}

pub fn write_aalv_file<P: AsRef<Path>>(path: P, aalvur: &Aalv) -> io::Result<File> {
    let mut f = File::options().create(true).write(true).open(path)?;
    aalvur.write(&mut f)?;
    Ok(f)
}

pub struct Aalv {
    pub file_offset: u64,
    segments: Vec<(Box<str>, Box<[u8]>)>,
}

impl Aalv {
    pub fn new() -> Self {
        Self { file_offset: 0, segments: Vec::new() }
    }

    pub fn read<R: BufRead + Seek>(mut reader: R) -> io::Result<Self> {
        let mut new = Self::new();
        new.file_offset = reader.stream_position()?;
        new.read_header(&mut reader)?;

        for (_, data) in &mut new.segments {
            reader.read_exact(data)?;
        }

        Ok(new)
    }

    pub fn write<W: Write + Seek>(&self, mut writer: W) -> io::Result<()> {
        writer.seek(SeekFrom::Start(self.file_offset))?;
        self.write_header(&mut writer)?;
        for (_name, data) in &self.segments {
            writer.write_all(&data)?;
        }

        Ok(())
    }

    /// Returns whether another segment with the same name already exists
    pub fn add_segment<S: Segment>(&mut self, segment: &S) -> Result<bool, S::Error> {
        assert!(!S::NAME.contains('\0'), "segment name may not contain null bytes");

        let mut buf = Vec::new();
        segment.write(&mut buf)?;

        let ret = self.segments.iter().any(|(s, _)| &**s == S::NAME);

        self.segments.push((
            S::NAME.into(),
            buf.into_boxed_slice()
        ));

        Ok(ret)
    }

    pub fn get_segment<S: Segment>(&self) -> Option<Result<S, S::Error>> {
        self.segments
            .iter()
            .find(|(s, _buf)| &**s == S::NAME)
            .map(|(_, buf)| S::read(buf))
    }

    fn read_header<R: BufRead + Seek>(&mut self, reader: &mut R) -> io::Result<()> {
        // Find magic
        loop {
            let first_magic_byte = AALV_HEADER.as_bytes()[0];
            let mut magic_buf = [0; AALV_HEADER.len()];

            while first_magic_byte != magic_buf[0] {
                reader.read_exact(&mut magic_buf[..1])?;
            }
            self.file_offset = reader.stream_position()? - 1;

            reader.read_exact(&mut magic_buf[1..])?;
            if &magic_buf == AALV_HEADER.as_bytes() {
                break;
            }
        }

        let mut name_buf = Vec::new();
        loop {
            name_buf.clear();
            reader.read_until(b'\0', &mut name_buf)?;

            if name_buf.pop() != Some(0) {
                return Err(io::Error::new(ErrorKind::InvalidData, "name did not end in a zero byte"));
            }

            let name: Box<str> = String::from_utf8_lossy(&name_buf).into();

            if name.len() == 0 {
                break;
            }

            let mut num_buf = [0; 4];
            reader.read_exact(&mut num_buf)?;

            let len = u32::from_le_bytes(num_buf) as usize;

            self.segments.push((name, vec![0; len].into_boxed_slice()));
        }

        Ok(())
   }

    fn write_header<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        writer.write_all(AALV_HEADER.as_bytes())?;
        for (name, data) in &self.segments {
            let data_len: [u8; 4] = (data.len() as u32).to_le_bytes();

            writer.write_all(name.as_bytes())?;
            writer.write_all(b"\0")?;
            writer.write_all(&data_len)?;
        }
        writer.write_all(b"\0")
    }
}

pub trait Segment: Sized {
    type Error;
    const NAME: &'static str;
    fn read(buf: &[u8]) -> Result<Self, Self::Error>;
    fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error>;
}

pub mod obj;
pub mod sample {
    use std::string::FromUtf8Error;

    use super::Segment;

    #[repr(transparent)]
    pub struct Name(pub String);
    
    impl Segment for Name {
        type Error = FromUtf8Error;
    
        const NAME: &'static str = "name";
    
        fn read(buf: &[u8]) -> Result<Self, Self::Error> {
            let v = buf.to_vec();
            Ok(Name(String::from_utf8(v)?))
        }
    
        fn write(&self, buf: &mut Vec<u8>) -> Result<(), Self::Error> {
            buf.extend_from_slice(self.0.as_bytes());
            Ok(())
        }
    }
}
