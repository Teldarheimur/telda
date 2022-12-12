use std::{fs::File, io::{Write, BufReader, BufRead, self}, path::Path, collections::HashMap, hash::Hash};

use crate::source::{LabelRead, Format};

pub const AALV_OBJECT_EXT: &str = "to";

#[deprecated]
pub fn read_symbol_file<T: SymbolCollector<String, u16>, U: SymbolCollector<u16, String>, P: AsRef<Path>>(path: P, labels: &mut T, pos_to_labels: &mut U) -> io::Result<()> {
    let f = File::open(path)?;

    for line in BufReader::new(f).lines() {
        let line = line?;
        let colon = line.find(':').unwrap();
        let lbl = line[..colon].to_owned();
        let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
        labels.insert(
            lbl.clone(),
            pos
        );
        pos_to_labels.insert(
            pos,
            lbl
        );
    }

    Ok(())
}

pub trait SymbolCollector<K, V> {
    fn insert(&mut self, k: K, v: V);
}

impl<K: Eq + Hash, V> SymbolCollector<K, V> for HashMap<K, V> {
    #[inline(always)]
    fn insert(&mut self, k: K, v: V) {
        HashMap::insert(self, k, v);
    }
}

impl<K, V> SymbolCollector<K, V> for () {
    #[inline(always)]
    fn insert(&mut self, _k: K, _v: V) {}
}

#[deprecated]
pub fn read_symbol_references<P: AsRef<Path>>(path: P) -> io::Result<Vec<(Format, Box<str>, u16)>> {
    let mut v = Vec::new();
    let f = File::open(path)?;

    for line in BufReader::new(f).lines() {
        let line = line?;
        let mode = &line[..2];
        let line = &line[2..];

        let at_sign = line.rfind(" @ ").unwrap();
        let lbl = line[..at_sign].into();
        let pos = u16::from_str_radix(&line[at_sign+5..], 16).unwrap();

        let mode = match mode {
            "A " => Format::Absolute,
            "B " => Format::Big,
            _ => unreachable!(),
        };

        v.push((mode, lbl, pos))
    }

    Ok(v)
}

#[deprecated]
pub fn write_symbol_references<'a, P: AsRef<Path>, I: 'a + Iterator<Item=(&'a str, &'a [LabelRead])>>(path: P, iter: I) -> io::Result<()> {
    let mut f = File::create(path)?;

    for (label, label_reads) in iter {
        for &LabelRead { position, format } in label_reads {
            match format {
                Format::Absolute => write!(f, "A ")?,
                Format::Big => write!(f, "B ")?,
            }
            writeln!(f, "{label} @ 0x{position:02x}")?;
        }
    }

    Ok(())
}
