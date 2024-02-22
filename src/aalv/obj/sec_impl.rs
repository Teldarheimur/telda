use std::io::{self, BufRead, BufReader, Read, Write};

use super::*;

impl Section for Entry {
    const NAME: &'static str = "_entry";
    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 3];
        reader.read_exact(&mut buf)?;
        let [stype, loc @ ..] = buf;
        Ok(Entry(segment_type_from_u8(stype)?, u16::from_le_bytes(loc)))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&[self.0 as u8])?;
        writer.write_all(&self.1.to_le_bytes())
    }
}

impl Section for Flags {
    const NAME: &'static str = "_flags";
    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;

        let mut flags = Flags::default();

        for c in buf.chars() {
            match c {
                'R' => flags.readable_text = true,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unrecognised flag",
                    ))
                }
            }
        }

        Ok(flags)
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        let &Flags { readable_text } = self;

        if readable_text {
            write!(writer, "R")?;
        }
        Ok(())
    }
}

impl Section for HeapSize {
    const NAME: &'static str = "_heap_size";
    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 2];
        reader.read_exact(&mut buf)?;
        Ok(HeapSize(u16::from_le_bytes(buf)))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&self.0.to_le_bytes())
    }
}

impl Section for BinarySegment {
    const NAME: &'static str = "_seg";

    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 3];
        reader.read_exact(&mut buf)?;
        let [ol, oh, stype] = buf;

        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes)?;

        Ok(Self {
            offset: u16::from_le_bytes([ol, oh]),
            stype: segment_type_from_u8(stype)?,
            bytes,
        })
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&self.offset.to_le_bytes())?;
        writer.write_all(&[self.stype as u8])?;
        writer.write_all(&self.bytes)?;
        Ok(())
    }
}
impl Section for StackSize {
    const NAME: &'static str = "_heap_size";
    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut buf = [0; 2];
        reader.read_exact(&mut buf)?;
        Ok(StackSize(u16::from_le_bytes(buf)))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&self.0.to_le_bytes())
    }
}

impl Section for SymbolTable {
    const NAME: &'static str = "_syms";

    fn read<R: Read>(reader: R) -> io::Result<Self> {
        let mut symbols = Vec::new();
        let mut reader = BufReader::new(reader);

        loop {
            let mut namebuf = Vec::new();
            let read_n = reader.read_until(0, &mut namebuf)?;
            if read_n == 0 {
                // EOF => we're done
                break;
            }
            namebuf.pop();

            let mut buf = [0; 3];
            reader.read_exact(&mut buf)?;
            let [stype, ol, oh] = buf;

            let segment_type = segment_type_from_u8(stype)?;

            let is_global = namebuf[0] != b' ';
            let name = if is_global {
                String::from_utf8_lossy(&namebuf).into()
            } else {
                String::from_utf8_lossy(&namebuf[1..]).into()
            };

            let def = SymbolDefinition {
                name,
                is_global,
                segment_type,
                location: u16::from_le_bytes([ol, oh]),
            };
            symbols.push(def);
        }

        Ok(SymbolTable(symbols))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        for &SymbolDefinition {
            ref name,
            is_global,
            segment_type,
            location,
        } in &self.0
        {
            write!(writer, "{}{name}\0", if is_global { "" } else { " " })?;
            writer.write_all(&[segment_type as u8])?;
            writer.write_all(&location.to_le_bytes())?;
        }

        Ok(())
    }
}

impl Section for RelocationTable {
    const NAME: &'static str = "_reloc";

    fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut entries = Vec::new();

        loop {
            let mut buf = [0; 5];
            match reader.read_exact(&mut buf) {
                Ok(()) => (),
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
                    break;
                }
                Err(e) => return Err(e),
            }
            let [stype, ol1, oh1, ol2, oh2] = buf;

            let reference_segment = segment_type_from_u8(stype)?;
            let reference_location = u16::from_le_bytes([ol1, oh1]);
            let symbol_index = u16::from_le_bytes([ol2, oh2]);

            let entry = RelocationEntry {
                reference_segment,
                reference_location,
                symbol_index,
            };
            entries.push(entry)
        }

        Ok(RelocationTable(entries))
    }
    fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        for &RelocationEntry {
            reference_segment,
            reference_location,
            symbol_index,
        } in &self.0
        {
            writer.write_all(&[reference_segment as u8])?;
            writer.write_all(&reference_location.to_le_bytes())?;
            writer.write_all(&symbol_index.to_le_bytes())?;
        }
        Ok(())
    }
}
